
module Trace ( withTrace
             , TraceLevel(..)
             , traceT
             , traceS
             , traceB) where

-- Execution tracing for multiple threads into file / stdout

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import Data.Time
import Data.List
import Text.Printf

data TraceLevel = TLNone | TLError | TLWarn | TLInfo deriving (Eq, Enum)

data TraceSettings = TraceSettings { tsFile   :: Maybe Handle
                                   , tsEchoOn :: Bool
                                   , tsLevel  :: TraceLevel
                                   }

-- The well-known unsafePerformIO hack. It would be a bit cumbersome to always pass the trace
-- record around or always be in a reader or special trace monad
{-# NOINLINE traceSettings #-}
traceSettings :: MVar TraceSettings
traceSettings = unsafePerformIO newEmptyMVar

withTrace :: Maybe FilePath -> Bool -> Bool -> TraceLevel -> IO () -> IO ()
withTrace traceFn echoOn appendOn level f =
    bracket
        ( do h <- case traceFn of Just fn -> if   level /= TLNone
                                             then Just <$> openFile fn (if   appendOn
                                                                        then AppendMode
                                                                        else WriteMode)
                                             else return Nothing
                                  _       -> return Nothing
             let ts = TraceSettings { tsFile   = h
                                    , tsEchoOn = echoOn
                                    , tsLevel  = level
                                    }
             r <- tryPutMVar traceSettings ts
             unless r $ error "Double initialization of Trace module"
             return ts
        )
        ( \ts -> do void . takeMVar $ traceSettings
                    case tsFile ts of Just h -> hClose h
                                      _      -> return ()
                    when (tsEchoOn ts) $ hFlush stdout
        )
        $ \_ -> f

trace :: TraceLevel -> T.Text -> IO ()
trace lvl msg = void $ withMVar traceSettings $ \ts -> -- TODO: Have to take an MVar even if
                                                       --       tracing is off, speed this up
   when (fromEnum lvl > 0 && fromEnum lvl <= (fromEnum $ tsLevel ts)) $ do
       tid  <- printf "%-12s" . show <$> myThreadId
       time <- show <$> getZonedTime
       let lvlDesc = case lvl of
                         TLError -> "ERROR  "
                         TLWarn  -> "WARNING"
                         TLInfo  -> "INFO   "
                         _       -> ""
           header  = concat $ intersperse " | " [ lvlDesc, tid, time ]
           handles = case tsFile   ts of   Just h -> [h];     _ -> []; ++
                     if   tsEchoOn ts then           [stdout] else []
       forM_ handles $ \h -> do
           hPutStrLn h header
           TI.hPutStrLn h msg
           hPutStrLn h ""

traceT :: TraceLevel -> T.Text -> IO ()
traceT lvl msg = trace lvl msg

traceS :: TraceLevel -> String -> IO ()
traceS lvl msg = trace lvl (T.pack msg)

traceB :: TraceLevel -> B.ByteString -> IO ()
traceB lvl msg = trace lvl (E.decodeUtf8 msg)

