
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module ProcessStatus ( processStatuses
                     , RetryAPI(..)
                     ) where

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.Attoparsec as CA
import Network.HTTP.Conduit
import qualified Web.Authenticate.OAuth as OA
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.List
import qualified Data.Vector as V
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent hiding (yield)
import Text.Printf
import Text.Read (readMaybe)
import System.IO

import TwitterJSON
import Trace

-- Pick up Twitter status updates and related messages from a file or an HTTP connection
-- and return the results as data structures from TwitterJSON

-- Put status messages in a TBQueue (TODO: Maybe use stm-conduit package?)
smQueueSink :: (MonadIO m, MonadResource m)
            => TBQueue StreamMessage
            -> Sink StreamMessage m ()
smQueueSink smQueue = awaitForever $ liftIO . atomically . writeTBQueue smQueue

-- Count bytes passing through using the state (need to be careful that we don't
-- retain a reference to the ByteString in case we're not looking at the state
-- for a while)
--
-- TODO: We take the byte count after gzip decompression, so not really what goes
--       over the socket
countBytesState :: (MonadIO m, MonadState Int m) => Conduit B.ByteString m B.ByteString
countBytesState = awaitForever $ \mi -> do
                      modify' (\x -> B.length mi `seq` x + fromIntegral (B.length mi))
                      yield mi

parseStatus :: (MonadIO m, MonadState Int m, MonadResource m)
            => Conduit B.ByteString m StreamMessage
parseStatus = do
    -- Bandwidth message for statistics
    yield <$> SMBytesReceived =<< get
    put 0
    -- Use conduit adapter for attoparsec to read the next JSON
    -- object with the Aeson parser from the stream connection
    --
    -- TODO: The performance we get is quite far away from the figures listed on
    --       Aeson's cabal site, do some benchmarking and profiling
    --
    j <- CA.sinkParser json'
    -- The stream connections send individual objects we can decode into SMs,
    -- but the home timeline sends an array of such objects
    let msg = case j of
                  Array v -> V.toList v
                  x       -> [x]
    forM_ msg $ \m -> do
        -- We expect something that can be parsed into a StreamMessage
        let sm = fromJSON m :: Result StreamMessage
        yield $ case sm of
            Success r -> r
            Error   s -> SMParseError $ B8.pack s
    parseStatus

-- Like conduitFile from Conduit.Binary, but with extra options to control the write
-- mode and flushing
conduitFileWithOptions :: MonadResource m
                       => FilePath
                       -> IOMode
                       -> Bool
                       -> Conduit B.ByteString m B.ByteString
conduitFileWithOptions fp mode flush =
    bracketP
        (openBinaryFile fp mode)
        hClose
        go
    where
        go h = awaitForever $ \bs -> liftIO (B.hPut h bs >> when flush (hFlush h)) >> yield bs

data RetryAPI = RetryNever
              | RetryNTimes Int
              | RetryForever -- This even retries in case of success
              deriving (Eq)

-- Process status updates as returned from the REST or Stream API, write results into a queue
processStatuses :: String
                -> OA.OAuth
                -> OA.Credential
                -> Manager
                -> Maybe FilePath
                -> Bool
                -> TBQueue StreamMessage
                -> RetryAPI
                -> IO ()
processStatuses uri oaClient oaCredential manager logFn appendLog smQueue retryAPI = do
  let uriIsHTTP = isPrefixOf "http://" uri || isPrefixOf "https://" uri -- File or HTTP?
  success <- catches
    ( -- We use State to keep track of how many bytes we received
      runResourceT . flip evalStateT (0 :: Int) $
          let sink' = countBytesState =$ parseStatus =$ smQueueSink smQueue
              sink  = case logFn of Just fn -> conduitFileWithOptions
                                                   fn
                                                   (if appendLog then AppendMode else WriteMode)
                                                   True
                                               =$ sink'
                                    Nothing -> sink'
          in  if   uriIsHTTP
              then do -- Authenticate, connect and start receiving stream
                      req' <- liftIO $ parseUrl uri
                      let req = req' { requestHeaders =
                                             ("Accept-Encoding", "deflate, gzip")
                                             -- Need a User-Agent field as well to get a
                                             -- gzip'ed stream from Twitter
                                           : ("User-Agent", "jacky/http-conduit")
                                           : requestHeaders req'
                                     }
                      reqSigned <- OA.signOAuth oaClient oaCredential req
                      liftIO . traceS TLInfo $ "Twitter API request:\n" ++ show reqSigned
                      res <- http reqSigned manager
                      liftIO . traceS TLInfo $ "Twitter API response from '" ++ uri ++ "'\n"
                                               ++ case logFn of Just fn -> "Saving full log in '"
                                                                           ++ fn ++ "'\n"
                                                                Nothing -> ""
                                               ++ "Status: " ++ show (responseStatus res)
                                               ++ "\n"
                                               ++ "Header: " ++ show (responseHeaders res)
                      -- Are we approaching the rate limit?
                      case find ((== "x-rate-limit-remaining") . fst) (responseHeaders res) >>=
                           readMaybe . B8.unpack . snd :: Maybe Int of
                               Just n  -> when (n < 5) . liftIO . traceS TLWarn $ printf
                                              "Rate limit remaining for API '%s' at %i" uri n
                               Nothing -> return ()
                      -- Finish parsing conduit
                      responseBody res $$+- sink
                      return True
              else do liftIO . traceS TLInfo $ "Streaming Twitter API response from file: " ++ uri
                      sourceFile uri $$ sink
                      return True
    )
    [ Handler $ \(ex :: AsyncException) ->
                   case ex of
                       ThreadKilled -> do
                           traceS TLInfo $ "Recv. ThreadKilled while processing statuses from '"
                                            ++ uri ++ "', exiting\n" ++ show ex
                           void $ throwIO ex -- Clean exit, but re-throw so we're not trapped
                                             -- inside RetryForever
                           return True
                       _            -> do
                           traceS TLError $ "Async exception while processing statuses from '"
                                            ++ uri ++ "\n" ++ show ex
                           return False
    , Handler $ \(ex :: HttpException) -> do
                   traceS TLError $ "HTTP / connection error while processing statuses from '"
                                    ++ uri ++ "'\n" ++ show ex
                   return False
    , Handler $ \(ex :: CA.ParseError) ->
                   if   "demandInput" `elem` CA.errorContexts ex
                   then do traceS TLInfo  $ "End-of-Data for '" ++ uri ++ "'\n" ++ show ex
                           return True -- This error is a clean exit
                   else do traceS TLError $ "JSON parser error while processing statuses from '"
                                           ++ uri ++ "'\n" ++ show ex
                           return False
    , Handler $ \(ex :: IOException) -> do
                   traceS TLError $ "IO error while processing statuses from '"
                                    ++ uri ++ "'\n" ++ show ex
                   return False -- TODO: This exception is likely caused by a failure to read
                                --       a network dump from disk, we might not want to bother
                                --       trying again if it can't be opened
    ]
  -- Do we need to retry?
  case retryAPI of
      RetryForever   -> when uriIsHTTP $ do -- Don't retry forever if our source is a static file
                            traceS TLWarn $
                                retryMsg ++ " (forever)\nURI: " ++ uri
                            threadDelay retryDelay
                            retryThis RetryForever
      RetryNTimes n  -> when (not success && n > 0) $ do
                            traceS TLWarn $ retryMsg ++ "\n"
                                            ++ "Remaining retries: " ++ show n ++ "\n"
                                            ++ "URI: " ++ uri
                            threadDelay retryDelay
                            retryThis (RetryNTimes $ n - 1)
      RetryNever     -> return ()
  where retryThis  = processStatuses
                         uri
                         oaClient
                         oaCredential
                         manager
                         logFn
                         True -- Don't overwrite log data we've got so far
                         smQueue
        retryDelay = 5 * 1000 * 1000 -- 5 seconds (TODO: Make this configurable)
        retryMsg   = printf "Retrying API request in %isec"
                            (retryDelay `div` 1000 `div` 1000)
 
