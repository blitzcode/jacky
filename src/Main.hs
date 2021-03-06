
{-# LANGUAGE   OverloadedStrings
             , FlexibleContexts
             , ScopedTypeVariables
             , RecordWildCards
             , NamedFieldPuns
             , LambdaCase #-}

module Main where

import System.IO
import System.Directory
import System.FilePath
import System.Exit
import qualified System.Info as SI
import Network.HTTP.Conduit
import Network.URI
-- import System.Remote.Monitoring
import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Data.List
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Exception
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Network (withSocketsDo)
import qualified GHC.Conc (getNumProcessors)
import qualified Codec.Picture as JP
import qualified Web.Authenticate.OAuth as OA

import App
import GLHelpers
import GLFWHelpers
import TwitterJSON
import ImageCache
import TextureCache
import CfgFile
import Trace
import ProcessStatus
import CmdLineOptDefinitions
import qualified BoundedSequence as BS
import Timing
import FontRendering
import QuadRendering

-- Parse command line options and setup State / Env for main application code

setupOAuth :: (MonadError String m, MonadIO m) => FilePath -> m (OA.OAuth, OA.Credential)
setupOAuth fn = do
    -- OAuth configuration file
    oauthCfg <- liftIO $ (loadCfgFile fn)
                `catch` (\(e :: IOException) -> print e >> return M.empty)
    when (M.null oauthCfg) $ throwError "Invalid OAuth configuration file"
    -- OAuth client and credentials
    let oaClient = OA.newOAuth
            { OA.oauthConsumerKey    = B8.pack $
                  M.findWithDefault "<NoKey>"    "oauth_consumer_key"    oauthCfg
            , OA.oauthConsumerSecret = B8.pack $
                  M.findWithDefault "<NoSecret>" "oauth_consumer_secret" oauthCfg
            }
        oaCredential = OA.newCredential
            (B8.pack $ M.findWithDefault "<NoToken>"  "oauth_token"        oauthCfg)
            (B8.pack $ M.findWithDefault "<NoSecret>" "oauth_token_secret" oauthCfg)
    return (oaClient, oaCredential)

runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

verifyImgCache :: FilePath -> IO ()
verifyImgCache folder = do
    images <- getDirectoryContents folder
    putStrLn $ "Reading & decoding " ++ show (length images - 2) ++ " images"
    forM_ images $ \fn' -> do
        let fn = folder </> fn'
        isDir <- doesDirectoryExist fn
        unless isDir $ do -- Skip directories
            di <- JP.readImage fn
            case di of
                Left err -> putStrLn $ "\n" ++ fn ++ ": " ++ err
                Right _  -> putStr "." >> hFlush stdout
    putStrLn ""

traceSystemInfo :: FontRenderer -> IO ()
traceSystemInfo ft2 = do
    cpus                       <- GHC.Conc.getNumProcessors
    (ft2maj, ft2min, ft2patch) <- getFT2Version ft2
    traceS TLInfo =<<
        ( (++) . concat . intersperse " · " $
             [ "System - OS: " ++ SI.os
             , "Arch: " ++ SI.arch
             , "CPUs: " ++ show cpus
             , concat [ "Compiler: "
                      , SI.compilerName
                      , " / "
                      , show SI.compilerVersion
                      ]
             , printf "FreeType Version: %i.%i.%i\n" ft2maj ft2min ft2patch
             ]
        )
        <$> getGLStrings

data LogNetworkMode = ModeNoLog | ModeLogNetwork | ModeReplayLog deriving (Eq, Enum, Show)

withProcessStatusesAsync :: OA.OAuth
                         -> OA.Credential
                         -> Manager
                         -> LogNetworkMode
                         -> String
                         -> TBQueue StreamMessage
                         -> String
                         -> RetryAPI
                         -> IO a
                         -> IO a
withProcessStatusesAsync oaClient
                         oaCredential
                         manager
                         logNetworkMode
                         logNetworkFolder
                         smQueue
                         uri'
                         retryAPI
                         f = do
    -- Pass the URI right through if we're not logging network data, add a log
    -- filename when logging is enabled, pass the log file as the URI when
    -- replaying previous log data
    --
    -- TODO: This will potentially overwrite older network logs of
    --       requests with identical parameters to identical API endpoints
    let logFn            = logNetworkFolder </> (escapeURIString isUnescapedInURIComponent uri')
        (uri, logFnMode) = case logNetworkMode of ModeNoLog      -> (uri' , Nothing   )
                                                  ModeLogNetwork -> (uri' , Just logFn)
                                                  ModeReplayLog  -> (logFn, Nothing   )
    -- Launch thread
    withAsync
        ( processStatuses
              uri
              oaClient
              oaCredential
              manager
              logFnMode
              False
              smQueue
              retryAPI
        )
        $ \_ -> f

main :: IO ()
main = do
    runOnAllCores -- Multicore
    -- Image cache folder
    cacheFolder <- addTrailingPathSeparator <$> getAppUserDataDirectory defImageCacheFolder
    let imgCacheFolder flagsArg = foldr
         (\f r -> case f of FlagImageCacheFolder fldr -> fldr; _ -> r) cacheFolder flagsArg
    res <- runExceptT $ do
        -- Process configuration / command line options
        flags <- parseCmdLineOpt
        when (FlagLogNetwork `elem` flags && FlagReplayLog `elem` flags) $
            throwError "Can't use -l and -r flags together"
        unless ((foldr (\f r -> case f of (FlagTraceLevel lvl) -> lvl; _ -> r) "n" flags)
                   `elem` ["n", "e", "w", "i"])
               $ throwError "Invalid trace level option (use [newi])"
        when (FlagVerifyImgCache `elem` flags) $ do
            liftIO . verifyImgCache $ imgCacheFolder flags
            throwError "Done, exiting"
        let oauthCfgFn = foldr (\f r -> case f of (FlagOAuthFile fn) -> fn; _ -> r)
                               "default.oauth"
                               flags
        (\(cl, cr) -> (flags, cl, cr)) <$> setupOAuth oauthCfgFn
        -- TODO: Plenty of flags get no checking
    (flags, oaClient, oaCredential) <- case res of
        Left  err  -> putStrLn ("Error: " ++ err) >> exitFailure
        Right r    -> return r
    -- Tracing (TODO: Change echo flag to specify separate trace level for stdout)
    let traceFn  = foldr (\f r -> case f of FlagTraceFile fn -> fn; _ -> r) defTraceFn flags
        mkTrcOpt = \case "n" -> TLNone; "e" -> TLError; "w" -> TLWarn; "i" -> TLInfo; _ -> TLNone
        traceLvl = foldr (\f r -> case f of (FlagTraceLevel lvl) -> mkTrcOpt lvl; _ -> r)
                         TLNone flags
    withTrace (Just traceFn)
              (      FlagTraceEchoOn       `elem` flags)
              (      FlagTraceAppend       `elem` flags)
              (not $ FlagTraceDisableColor `elem` flags)
              traceLvl
              $ do
      mapM_ (traceS TLInfo) [ show flags, show oaClient, show oaCredential ]
      -- Make sure the network log file folder exists, if logging is requested
      let logNetworkMode | FlagLogNetwork `elem` flags = ModeLogNetwork
                         | FlagReplayLog  `elem` flags = ModeReplayLog
                         | otherwise                   = ModeNoLog
          logNetworkFolder =
              foldr (\f r -> case f of FlagLogFolder folder -> folder; _ -> r) defLogFolder flags
      when (logNetworkMode == ModeLogNetwork) $
          createDirectoryIfMissing True logNetworkFolder
      -- Image cache concurrent fetches
      let concImgFetches = foldr (\f r -> case f of
           FlagConcImgFetches n -> fromMaybe r $ readMaybe n
           _                    -> r)
           defConcImgFetches flags
      withSocketsDo $
        let managerConnCount = foldr (\f r -> case f of
                FlagConKeepAlive n -> fromMaybe r $ readMaybe n
                _                  -> r)
                defConKeepAlive flags
            managerResponseTimeout = Just $ foldr (\f r -> case f of
                FlagConTimeout n -> fromMaybe r $ readMaybe n
                _                -> r)
                defConTimeout flags
        in  newManager tlsManagerSettings { managerConnCount, managerResponseTimeout }
                >>= \manager -> liftIO $ do
          -- Launch thread(s) for parsing status updates
          envSMQueue <- newTBQueueIO 1024 :: IO (TBQueue StreamMessage)
          let withPSAsync f =
                  if   FlagFirehose `elem` flags
                  then withProcessStatusesAsync'
                           twitterStatusesRandomStreamURL
                           RetryForever
                           f
                  else withProcessStatusesAsync'
                           twitterUserStreamURL
                           RetryForever
                           . withProcessStatusesAsync'
                                 (twitterHomeTimeline ++ "?count=200")
                                 (RetryNTimes 5)
                                 $ f
              withProcessStatusesAsync' = withProcessStatusesAsync oaClient
                                                                   oaCredential
                                                                   manager
                                                                   logNetworkMode
                                                                   logNetworkFolder
                                                                   envSMQueue
           in withPSAsync $
            let cacheSize = foldr (\f r -> case f of
                    FlagImgMemCacheSize n -> fromMaybe r $ readMaybe n
                    _                     -> r)
                    defImgMemCacheSize flags
            in  withImageCache manager
                               (cacheSize `div` 2)
                               concImgFetches
                               (imgCacheFolder flags)
                               $ \envImageCache -> do
              -- Event queues filled by GLFW callbacks
              envGLFWEventsQueue <- newTQueueIO :: IO (TQueue GLFWEvent)
              let wndWdh = 1280
                  wndHgt = 644
               in withWindow wndWdh wndHgt "Twitter" envGLFWEventsQueue $ \envWindow ->
                let texPackSize = foldr (\f r ->case f of
                        FlagTexturePackSize n -> fromMaybe r $ readMaybe n
                        _ -> r) defTexturePackSize flags
                in  withTextureCache cacheSize
                                     True -- Use texture grid packing
                                     texPackSize
                                     (80, 80) -- Twitter avatar images are smaller than this
                                     envImageCache
                                     $ \envTextureCache ->
                  let maxQuad = foldr (\f r -> case f of
                          FlagQuadRBSize n -> fromMaybe r $ readMaybe n
                          _ -> r) defQuadRBSize flags
                  in withQuadRenderer maxQuad $ \envQuadRenderer ->
                    withFontRenderer (FlagForceAutohint `elem` flags)
                                     (FlagDisableKern   `elem` flags)
                                     True -- Use a texture atlas
                                     texPackSize
                                     $ \envFontRenderer -> do
                      when (FlagFT2Test `elem` flags) $ debugPrintTest envFontRenderer
                      traceSystemInfo envFontRenderer
                      -- Start EKG server (disabled for now)
                      -- ekg <- forkServer "localhost" 8000
                      -- Setup reader and state for main AppDraw monad
                      stCurTick <- getTick
                      let envInit = Env
                            { envTweetHistSize           = foldr (\f r -> case f of
                                                                    FlagTweetHistory n ->
                                                                        fromMaybe r $ readMaybe n
                                                                    _ -> r)
                                                                 defTweetHistory flags
                            , envStatTraceInterval       = foldr (\f r -> case f of
                                                                    FlagStatTraceInterval n ->
                                                                        fromMaybe r $ readMaybe n
                                                                    _ -> r)
                                                                 defStatTraceInterval flags
                            , envDumpFT2AtlasOnTrace     = FlagDumpFT2AtlasOnTrace     `elem` flags
                            , envDumpTexCacheGridOnTrace = FlagDumpTexCacheGridOnTrace `elem` flags
                            , ..
                            }
                          stateInit = State
                            { stTweetByID          = M.empty
                            , stUILayoutRects      = []
                            , stFrameTimes         = -- FPS History for stat trace interval
                                                     BS.empty . round $
                                                         60 * envStatTraceInterval envInit
                            , stLastStatTrace      = stCurTick
                            , stLastEscPress       = -1
                            , stStatTweetsReceived = 0
                            , stStatDelsReceived   = 0
                            , stStatBytesRecvAPI   = 0
                            , ..
                            }
                      -- Enter main loop
                      void . flip runReaderT envInit . flip runStateT stateInit $ run
      traceS TLInfo "Clean Exit"

