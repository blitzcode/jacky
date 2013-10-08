
{-# LANGUAGE PackageImports, OverloadedStrings, FlexibleContexts #-}

module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import System.IO.Error
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Network (withSocketsDo) 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Control.Concurrent hiding (yield)
import qualified GHC.Conc (getNumProcessors)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad.RWS.Strict
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import System.IO
import Data.Int
import Data.Maybe
import Data.Word
import qualified Data.Text as T
import System.Directory
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPT
import qualified Web.Authenticate.OAuth as OA
import Network.HTTP.Conduit
import Network.URI
import System.FilePath

import CfgFile
import TwitterJSON
import HTTPImageCache
import Log
import ProcessStatus
import Util (modify')
import GLHelpers

-- TODO: Use bracket or ResourceT in the various withXYZ functions
-- TODO: Start using Lens library for records and Reader/State

{- Unicode Char Dist Test
 6537
 Stack space overflow: current size 8388608 bytes.
Use `+RTS -Ksize -RTS' to increase it.
-}

data LogNetworkMode = ModeNoLog | ModeLogNetwork | ModeReplayLog deriving (Eq, Enum, Show)

data Env = Env
    { envWindow           :: GLFW.Window
    , envGLFWEventsQueue  :: TQueue GLFWEvent
    , envSMQueue          :: TBQueue StreamMessage
    , envHTTPImageCache   :: HTTPImageCache GL.TextureObject
    , envLogNetworkFolder :: String
    , envLogNetworkMode   :: LogNetworkMode
    , envOAClient         :: OA.OAuth
    , envOACredential     :: OA.Credential
    , envManager          :: Manager
    }

data State = State
    { stTweetByID        :: M.Map Int64 Tweet
    --, stTweetByCreatedAt :: Set.Set TweetByCreatedAt
    , stDbgChars :: Set.Set Int
    }

type AppDraw = RWST Env () State IO

-- Convenience function printing out information about IO file errors and
-- returning a default
printIOError :: IO a -> a -> IO a
printIOError io errDef =
    catchIOError io $ \e -> do
        putStrLn $ fromMaybe "" ((++ " ") <$> ioeGetFileName e) ++ ioeGetErrorString e
        return errDef

data Flag = FlagOAuthFile String
          | FlagHelp
          | FlagLogNetwork
          | FlagReplayLog
          | FlagLogFolder String
          | FlagHTTPImageCacheFolder String
          | FlagConcImgFetches String
          | FlagVerifyImgCache
            deriving Eq

defLogFolder, defHTTPImageCacheFolder :: String
defLogFolder = "./log/"
defHTTPImageCacheFolder = "http_img_cache"
defConcImgFetches :: Int
defConcImgFetches = 50

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
                           (ReqArg FlagHTTPImageCacheFolder "FOLDER")
                           ("location of HTTP image cache (default: '"
                               ++ defHTTPImageCacheFolder ++ "' in HOME)")
                  , Option ['n']
                           ["conc-img-fetches"]
                           (ReqArg FlagConcImgFetches "NUMBER")
                           ("number of concurrent image fetches (default: "
                               ++ show defConcImgFetches ++ ")")
                  , Option []
                           ["verify-img-cache"]
                           (NoArg FlagVerifyImgCache)
                           "Debug: Try to read & decompress all images in the cache"
                  , Option ['h']
                           ["help"]
                           (NoArg FlagHelp)
                           "print usage information"
                  ]

setupOAuth :: (MonadError String m, MonadIO m) => FilePath -> m (OA.OAuth, OA.Credential)
setupOAuth fn = do
    -- OAuth configuration file
    oauthCfg <- liftIO $ (loadCfgFile fn) `printIOError` M.empty
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

draw :: AppDraw ()
draw = do
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 0.2 0.2 0.2 0.0 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer]
{-
    GLU.build2DMipmaps
        GL.Texture2D GL.RGBA' (fromIntegral fontTexWdh) (fromIntegral fontTexWdh)
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
-}
    tweets <- gets stTweetByID
    hic    <- asks envHTTPImageCache
    forM_ (zip ([0..349] :: [Int]) (reverse . Prelude.take 349 $ M.toDescList tweets)) $ \(idx, (_, tw)) -> liftIO $ do
        ce <- fetchImage hic (usrProfileImageURL . twUser $ tw)
        case ce of
            Just (Fetched (HTTPImageRes w h img)) -> do
                GL.windowPos (GL.Vertex2
                    (fromIntegral $ (idx `mod` 25) * 47)
                    (fromIntegral $ (idx `div` 25) * 47)
                    :: GL.Vertex2 GL.GLint)
                when (VS.length img /= w * h) $
                    error "HTTPImageRes storage / dimensions mismatch"
                VS.unsafeWith img $ \ptr ->
                    GL.drawPixels
                        (GL.Size (fromIntegral w) (fromIntegral h))
                        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
            Just (Processed tex) -> return ()
            _ -> return ()

-- Process all available events in both bounded and unbounded STM queues
processAllEvents :: (MonadIO m) => Either (TQueue a) (TBQueue a) -> (a -> m ()) -> m ()
processAllEvents tq processEvent = do
    me <- liftIO . atomically $ case tq of Left  tqUnbounded -> tryReadTQueue  tqUnbounded
                                           Right tqBounded   -> tryReadTBQueue tqBounded
    case me of
        Just e -> processEvent e >> processAllEvents tq processEvent
        _      -> return ()

processGLFWEvent :: GLFWEvent -> AppDraw ()
processGLFWEvent ev =
    case ev of
        (GLFWEventError e s) -> do
            window <- asks envWindow
            liftIO $ do
                putStrLn $ "error " ++ show e ++ " " ++ show s
                GLFW.setWindowShouldClose window True
        (GLFWEventKey window k _ ks mk) ->
            when (ks == GLFW.KeyState'Pressed) $ do
                when (k == GLFW.Key'Escape) $
                    liftIO $ GLFW.setWindowShouldClose window True

highResProfileImgURL :: B.ByteString -> B.ByteString
highResProfileImgURL url =
    let (h, t') = B.breakSubstring "_normal." url
        (_, t)  = B.breakSubstring "." t'
    in  if   not $ B.null t
        then h <> "_bigger" <> t
        else url

processSMEvent :: StreamMessage -> AppDraw ()
processSMEvent ev =
    case ev of
        (SMParseError bs) -> liftIO . putStrLn $ "\nStream Parse Error: " ++ B8.unpack bs
        SMTweet tw' ->
            do hic <- asks envHTTPImageCache
               -- Always try to fetch the higher resolution profile images
               let tw = tw' { twUser = (twUser tw')
                                  { usrProfileImageURL =
                                        highResProfileImgURL (usrProfileImageURL . twUser $ tw')
                                  }
                            }

               --liftIO . void $ fetchImage hic (usrProfileImageURL . twUser $ tw)
               modify' $ \s -> s { stTweetByID = M.insert (twID tw) tw (stTweetByID s) }
               -- TODO: Delete oldest tweet when we reached a limit

{-
               let nChars = Set.fromList . map fromEnum . T.unpack $ twText tw
               nChars `seq` modify' $ \s ->
                   s { stDbgChars = Set.union (stDbgChars s) nChars }
-}             

               --cset <- gets stDbgChars
               --liftIO $ (putStr $ show (Set.size cset) ++ " ") >> hFlush stdout

               {-
               modify' $ \s -> s { stTweetByCreatedAt = Set.insert
                                       (TweetByCreatedAt tw)
                                       (stTweetByCreatedAt s)
                                 }
               -}
               --return ()
        SMDelete _ _ -> return ()
        -- Debug print all other messages
        _  -> liftIO $ (putStr $ (Prelude.head . words . show $ ev) ++ " ") >> hFlush stdout

processStatusesAsync :: String -> AppDraw ()
processStatusesAsync uri' = do
    manager          <- asks envManager
    oaClient         <- asks envOAClient
    oaCredential     <- asks envOACredential
    smQueue          <- asks envSMQueue
    logNetworkFolder <- asks envLogNetworkFolder
    logNetworkMode   <- asks envLogNetworkMode
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
    void . liftIO . forkIO $
        processStatuses
            uri
            oaClient
            oaCredential
            manager
            logFnMode
            smQueue
        
run :: AppDraw ()
run = do
    -- Setup OpenGL / GLFW
    glfwEventsQueue <- asks envGLFWEventsQueue
    window          <- asks envWindow
    liftIO $ do
        (w, h) <- GLFW.getWindowSize window
        GLFW.setErrorCallback        . Just $ errorCallback glfwEventsQueue
        GLFW.setKeyCallback   window . Just $ keyCallback   glfwEventsQueue
        GLFW.swapInterval 1
        setup2DOpenGL w h
    -- Launch thread for parsing status updates
    processStatusesAsync
        twitterStatusesRandomStreamURL
    {-
    processStatusesAsync
        twitterUserStreamURL
    processStatusesAsync $
        twitterHomeTimeline ++ "?count=200"
    -}
    -- Main loop
    let loop = do
        -- Stream messages
        tqSM <- asks envSMQueue
        processAllEvents (Right tqSM) processSMEvent
        -- GLFW / OpenGL
        draw
        liftIO $ do
            GLFW.swapBuffers window
            GLFW.pollEvents
            err <- GL.get GL.errors
            mapM_ print err
        tqGLFW <- asks envGLFWEventsQueue
        processAllEvents (Left tqGLFW) processGLFWEvent
        -- Done?
        close <- liftIO $ GLFW.windowShouldClose window
        unless close loop
     in loop

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

main :: IO ()
main = do
    runOnAllCores -- Multicore
    -- HTTP image cache folder
    cacheFolder <- addTrailingPathSeparator <$> getAppUserDataDirectory defHTTPImageCacheFolder
    let imgCacheFolder flagsArg = foldr
         (\f r -> case f of FlagHTTPImageCacheFolder fldr -> fldr; _ -> r) cacheFolder flagsArg
    res <- runErrorT $ do
        -- Process configuration / command line options
        flags <- parseCmdLineOpt
        when (FlagLogNetwork `elem` flags && FlagReplayLog `elem` flags) $
            throwError "Can't use -l and -r flags together"
        when (FlagVerifyImgCache `elem` flags) $ do
            liftIO . verifyImgCache $ imgCacheFolder flags
            throwError "Done, exiting"
        let oauthCfgFn = foldr (\f r -> case f of (FlagOAuthFile fn) -> fn; _ -> r)
                               "default.oauth"
                               flags
        (\(cl, cr) -> (flags, cl, cr)) <$> setupOAuth oauthCfgFn
    (flags, oaClient, oaCredential) <- case res of
        Left  err  -> putStrLn ("Error: " ++ err) >> exitFailure
        Right r    -> return r
    -- Make sure the network log file folder exists, if logging is requested
    let logNetworkMode | FlagLogNetwork `elem` flags = ModeLogNetwork
                       | FlagReplayLog  `elem` flags = ModeReplayLog
                       | otherwise                   = ModeNoLog
        logNetworkFolder =
            foldr (\f r -> case f of FlagLogFolder folder -> folder; _ -> r) defLogFolder flags
    when (logNetworkMode == ModeLogNetwork) $
        createDirectoryIfMissing True logNetworkFolder
    -- HTTP image cache concurrent fetches
    let concImgFetches = foldr (\f r -> case f of
         FlagConcImgFetches n -> fromMaybe r (fst <$> (listToMaybe . reads) n :: Maybe Int)
         _                    -> r)
         defConcImgFetches
         flags
    -- Network initialization
    withSocketsDo $
      -- Initialize http-conduit
      withManagerSettings (def { managerConnCount = 10 }) $ \manager ->
        -- Initialize HTTP image cache
        withHTTPImageCache manager concImgFetches (imgCacheFolder flags) $ \hic -> liftIO $
          -- Initialize GLFW
          withWindow 1175 658 "Twitter" $ \window -> do
              -- Event queues filled by GLFW callbacks, stream messages
              initGLFWEventsQueue <- newTQueueIO       :: IO (TQueue  GLFWEvent)
              initSMQueue         <- newTBQueueIO 1024 :: IO (TBQueue StreamMessage)
              -- Start main loop in RWS / IO monads
              let envInit = Env
                      { envWindow           = window
                      , envGLFWEventsQueue  = initGLFWEventsQueue
                      , envSMQueue          = initSMQueue
                      , envHTTPImageCache   = hic
                      , envLogNetworkFolder = logNetworkFolder
                      , envLogNetworkMode   = logNetworkMode
                      , envOAClient         = oaClient
                      , envOACredential     = oaCredential
                      , envManager          = manager
                      }
                  stateInit = State
                      { stTweetByID        = M.empty
                      --, stTweetByCreatedAt = Set.empty
                      , stDbgChars = Set.empty
                      }
              void $ evalRWST run envInit stateInit
    putStrLn "Done"

