
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module CmdLineOptDefinitions where -- Export everything

import System.Console.GetOpt
import System.Environment
import Control.Monad.IO.Class
import Control.Monad.Error

-- Command line flag / option parsing, defaults

data Flag = FlagOAuthFile String
          | FlagHelp
          | FlagLogNetwork
          | FlagReplayLog
          | FlagLogFolder String
          | FlagImageCacheFolder String
          | FlagConcImgFetches String
          | FlagVerifyImgCache
          | FlagTraceFile String
          | FlagTraceLevel String
          | FlagTraceEchoOn
          | FlagTraceAppend
          | FlagConKeepAlive String
          | FlagConTimeout String
          | FlagTweetHistory String
          | FlagImgMemCacheSize String
          | FlagStatTraceInterval String
          | FlagFirehose
          | FlagTraceDisableColor
            deriving (Eq, Show)

defLogFolder, defImageCacheFolder, defTraceFn :: String
defLogFolder = "./log/"
defImageCacheFolder = "http_img_cache"
defTraceFn = "./trace.log"
defConcImgFetches :: Int
defConcImgFetches = 50
defConKeepAlive :: Int
defConKeepAlive = 10
defConTimeout :: Int
defConTimeout = 5 * 1000 * 1000
defTweetHistory :: Int
defTweetHistory = 1024
defImgMemCacheSize :: Int
defImgMemCacheSize = 1024
defStatTraceInterval = 10.0
defStatTraceInterval :: Double

parseCmdLineOpt :: (MonadError String m, MonadIO m) => m [Flag]
parseCmdLineOpt = do
    name <- liftIO $ getProgName
    args <- liftIO $ getArgs
    let header = "Usage: " ++ name ++ " [OPTION...]"
        usage  = usageInfo header options
    flags <- case getOpt Permute options args of
                 (f , [], [] ) -> return f
                 (_ , _ , err) -> throwError ("\n" ++ concat err ++ usage) >> return []
    when (FlagHelp `elem` flags) $ (liftIO $ putStrLn usage) >> throwError "Done, exiting"
    return flags
    where
        options :: [OptDescr Flag]
        options = [ Option ['o']
                           ["oauth"]
                           (ReqArg FlagOAuthFile "FILE")
                           (  "load OAuth details from FILE\n"
                           ++ "expected format / contents:\n"
                           ++ "  oauth_consumer_key    = ...\n"
                           ++ "  oauth_consumer_secret = ...\n"
                           ++ "  oauth_token           = ...\n"
                           ++ "  oauth_token_secret    = ...\n"
                           ++ "see https://dev.twitter.com/apps/new"
                           )
                  , Option []
                           ["firehose"]
                           (NoArg FlagFirehose)
                           "connect to firehose instead of user home timeline"
                  , Option ['f']
                           ["log-folder"]
                           (ReqArg FlagLogFolder "FOLDER")
                           ("location of network data dump (default: " ++ defLogFolder ++ ")")
                  , Option ['l']
                           ["log-network"]
                           (NoArg FlagLogNetwork)
                           "dump all received API network data in log folder"
                  , Option ['r']
                           ["replay-log"]
                           (NoArg FlagReplayLog)
                           "replay dumped API network data from log folder"
                  , Option ['i']
                           ["http-imgcache-folder"]
                           (ReqArg FlagImageCacheFolder "FOLDER")
                           ("image cache fldr (default: '"
                               ++ defImageCacheFolder ++ "' in HOME)")
                  , Option []
                           ["conc-img-fetches"]
                           (ReqArg FlagConcImgFetches "NUMBER")
                           ("number of concurrent image fetches (default: "
                               ++ show defConcImgFetches ++ ")")
                  , Option ['m']
                           ["img-mem-cache-size"]
                           (ReqArg FlagImgMemCacheSize "NUMBER")
                           ("number of images to keep in mem cache (default: "
                               ++ show defImgMemCacheSize ++ ")")
                  , Option []
                           ["conn-keep-alive"]
                           (ReqArg FlagConKeepAlive "NUMBER")
                           ("# of conn. to single host to keep alive (default: "
                               ++ show defConKeepAlive ++ ")")
                  , Option []
                           ["conn-timeout"]
                           (ReqArg FlagConTimeout "NUMBER")
                           ("HTTP request timeout (in µs, default: "
                               ++ show defConTimeout ++ ")")
                  , Option ['s']
                           ["stat-trace-interval"]
                           (ReqArg FlagStatTraceInterval "NUMBER")
                           ("statistics tracing interval (in seconds, default: "
                               ++ show defStatTraceInterval ++ ")")
                  , Option ['n']
                           ["tweet-hist"]
                           (ReqArg FlagTweetHistory "NUMBER")
                           ("number of tweets to keep around (default: "
                               ++ show defTweetHistory ++ ")")
                  , Option []
                           ["trace-file"]
                           (ReqArg FlagTraceFile "FILE")
                           ("output file for execution trace (default: " ++ defTraceFn ++ ")")
                  , Option ['t']
                           ["trace-level"]
                           (ReqArg FlagTraceLevel "LEVEL")
                           (  "execution trace level (default: n)\n"
                           ++ "  n = none\n"
                           ++ "  e = errors only\n"
                           ++ "  w = warnings and errors\n"
                           ++ "  i = infos, warnings and errors"
                           )
                  , Option ['e']
                           ["trace-echo"]
                           (NoArg FlagTraceEchoOn)
                           ("echo execution trace to stdout as well")
                  , Option []
                           ["trace-append"]
                           (NoArg FlagTraceAppend)
                           ("append execution trace file instead of overwriting")
                  , Option []
                           ["trace-no-color"]
                           (NoArg FlagTraceDisableColor)
                           ("no ANSI colors for trace output (default: on)")
                  , Option []
                           ["verify-img-cache"]
                           (NoArg FlagVerifyImgCache)
                           "debug: try to read & decode all cached images"
                  , Option ['h']
                           ["help"]
                           (NoArg FlagHelp)
                           "print usage information"
                  -- TODO: Option for offline mode
                  -- TODO: Option to discard caches
                  ]

