
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module ProcessStatus ( processStatuses
                     ) where

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.Attoparsec as CA
import Network.HTTP.Conduit
import qualified Web.Authenticate.OAuth as OA
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.Word
import Data.List
import qualified Data.Vector as V
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Concurrent.STM
import Control.Exception

import TwitterJSON
import Util (modify')
import Trace

-- Pick up Twitter status updates and related messages from a file or an HTTP connection
-- and return the results as data structures from TwitterJSON

-- Put status messages in a TBQueue (TODO: Maybe use stm-conduit package?)
smQueueSink :: (MonadIO m, MonadResource m)
            => TBQueue StreamMessage
            -> Sink StreamMessage m ()
smQueueSink smQueue = awaitForever $ liftIO . atomically . writeTBQueue smQueue
-- TODO: Sent byte count messages

-- Count bytes passing through using the state (need to be careful that we don't
-- retain a reference to the ByteString in case we're not looking at the state
-- for a while)
countBytesState :: (  MonadIO m {-@-}  , MonadState Word64 m) => Conduit B.ByteString m B.ByteString
countBytesState = awaitForever $ \mi -> do
                      modify' (\x -> B.length mi `seq` x + (fromIntegral $ B.length mi))
                      --liftIO $ B.putStrLn mi {-@-}
                      yield mi

parseStatus :: (MonadIO m, MonadResource m) => Conduit B.ByteString m StreamMessage
parseStatus = do
    -- Use conduit adapter for attoparsec to read the next JSON
    -- object with the Aeson parser from the stream connection
    --
    -- TODO: Use strict json' instead?
    -- TODO: This eventually fails with a ParseError / "not enough bytes", figure out
    --       how to exit cleanly when there is no more input
    j <- CA.sinkParser json
    -- The stream connections send individual objects we can decode into SMs,
    -- but the home timeline sends an array of such objects
    let msg = case j of
                  Array (v) -> V.toList v
                  x         -> [x]
    forM_ msg $ \m -> do
        -- We expect something that can be parsed into a StreamMessage
        let sm = fromJSON m :: Result StreamMessage
        yield $ case sm of
            Success r -> r
            Error   s -> SMParseError $ B8.pack s
    parseStatus

-- Process status updates as returned from the REST or Stream API, write results into a queue
processStatuses :: String
                -> OA.OAuth
                -> OA.Credential
                -> Manager
                -> Maybe FilePath
                -> TBQueue StreamMessage
                -> IO ()
processStatuses uri oaClient oaCredential manager logFn smQueue =
  catches
    ( do
     -- We use State to keep track of how many bytes we received
     runResourceT . flip evalStateT (0 :: Word64) $
         let sink' = countBytesState =$ parseStatus =$ smQueueSink smQueue
             sink  = case logFn of Just fn -> conduitFile fn =$ sink' -- TODO: Doesn't flush on crash
                                   Nothing -> sink'
         in  if   isPrefixOf "http://" uri || isPrefixOf "https://" uri -- File or HTTP?
             then do -- Authenticate, connect and start receiving stream
                     req' <- liftIO $ parseUrl uri
                     let req = req' { requestHeaders =
                                            ("Accept-Encoding", "deflate, gzip")
                                            -- Need a User-Agent field as well to get a
                                            -- gzip'ed stream from Twitter
                                          : ("User-Agent", "http-conduit")
                                          : requestHeaders req'
                                    }
                     reqSigned <- OA.signOAuth oaClient oaCredential req
                     liftIO . traceS TLInfo $ "Twitter API request:\n" ++ show reqSigned
                     res <- http reqSigned manager
                     -- TODO: Have a look at the rate limit response field
                     liftIO . traceS TLInfo $ "Twitter API response from '" ++ uri ++ "'\n"
                                              ++ case logFn of Just fn -> "Saving full log in '"
                                                                          ++ fn ++ "'\n"
                                                               Nothing -> ""
                                              ++ "Status: " ++ show (responseStatus res)
                                              ++ "\n"
                                              ++ "Header: " ++ show (responseHeaders res)
                     -- Finish parsing conduit
                     responseBody res $$+- sink
             else do liftIO . traceS TLInfo $ "Streaming Twitter API response from file: " ++ uri
                     sourceFile uri $$ sink
    )
    -- TODO: Decide which errors are recoverable and retry after a waiting for a few seconds
    [ Handler (\(ex :: HttpException) ->
                  traceS TLError $ "HTTP / connection error while processing statuses from '"
                                   ++ uri ++ "'\n" ++ show ex
              )
    , Handler (\(ex :: CA.ParseError) -> 
                  traceS TLError $ "JSON parser error while processing statuses from '"
                                   ++ uri ++ "'\n" ++ show ex
              )
    ]

