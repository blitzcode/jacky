
{-# LANGUAGE   PackageImports
             , OverloadedStrings
             , FlexibleContexts
             , ScopedTypeVariables
             , LambdaCase #-}

module Main where

import System.Exit
import Control.Applicative
import qualified Data.Map.Strict as M
import Control.Monad.Error
import Network (withSocketsDo) 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import qualified GHC.Conc (getNumProcessors)
import Control.Concurrent.STM
import Control.Monad.RWS.Strict
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import System.IO
import Data.Int
import Data.Maybe
import System.Directory
import qualified Codec.Picture as JP
import qualified Web.Authenticate.OAuth as OA
import Network.HTTP.Conduit
import Network.URI
import System.FilePath
import Text.Printf
import Control.Exception
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Control.Monad.Trans.Control

import CfgFile
import TwitterJSON
import ImageCache
import Trace
import ProcessStatus
import Util
import GLHelpers
import TextureCache
import qualified RectPacker as RP
import BoundedSequence as BS
import CmdLineOptDefinitions

-- TODO: Start using Lens library for records and Reader/State
-- TODO: Use labelThread for all threads

data LogNetworkMode = ModeNoLog | ModeLogNetwork | ModeReplayLog deriving (Eq, Enum, Show)

data Env = Env
    { envWindow            :: GLFW.Window
    , envGLFWEventsQueue   :: TQueue GLFWEvent
    , envSMQueue           :: TBQueue StreamMessage
    , envImageCache        :: ImageCache
    , envTextureCache      :: TextureCache
    , envLogNetworkFolder  :: String
    , envLogNetworkMode    :: LogNetworkMode
    , envOAClient          :: OA.OAuth
    , envOACredential      :: OA.Credential
    , envManager           :: Manager
    , envTweetHistSize     :: Int
    , envStatTraceInterval :: Double
    }

data State = State
    { stTweetByID          :: M.Map Int64 Tweet
    , stUILayoutRects      :: [(Int, Int, Int, Int)]
      -- Statistics
    , stFrameTimes         :: BS.BoundedSequence Double
    , stLastStatTrace      :: Double
    , stStatTweetsReceived :: Int
    , stStatDelsReceived   :: Int
    , stStatBytesRecvAPI   :: Int
    }

type AppDraw = RWST Env () State IO


color3f :: Float -> Float -> Float -> IO ()
color3f r g b = GL.color $ GL.Color3 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b)

color4f :: Float -> Float -> Float -> Float -> IO ()
color4f r g b a = GL.color $
    GL.Color4 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b) (realToFrac a)

vertex2f :: Float -> Float -> IO ()
vertex2f x y = GL.vertex $ GL.Vertex2 (realToFrac x :: GL.GLfloat) (realToFrac y)

texCoord2f :: Float -> Float -> IO ()
texCoord2f u v = GL.texCoord $ GL.TexCoord2 (realToFrac u :: GL.GLfloat) (realToFrac v)

drawQuad :: Float -> Float -> Float -> Float -> (Float, Float, Float) -> IO ()
drawQuad x y w h (r, g, b) =
    GL.preservingMatrix $ do
        Just time <- GLFW.getTime
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate $
            GL.Vector3 (realToFrac $ x + w/2) (realToFrac $ y + h/2) (0.0 :: GL.GLfloat)
        GL.rotate (realToFrac time * 30 + (realToFrac $ x+y) :: GL.GLfloat) $ GL.Vector3 0.0 0.0 1.0
        GL.renderPrimitive GL.Quads $ do
            color3f r g b
            texCoord2f 0.0 0.0
            vertex2f (-w / 2) (-h / 2)
            texCoord2f 1.0 0.0
            vertex2f (w / 2) (-h / 2)
            texCoord2f 1.0 1.0
            vertex2f (w / 2) (h / 2)
            texCoord2f 0.0 1.0
            vertex2f (-w / 2) (h / 2)

draw :: AppDraw ()
draw = do
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 0.2 0.2 0.2 0.0 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer]

    tweets <- gets stTweetByID
    tiles  <- gets stUILayoutRects
    tc     <- asks envTextureCache

    forM_ (zip tiles (M.toDescList tweets)) $ \((cx, cy, cw, ch), (_, tw)) -> do
        ce <- liftIO $ TextureCache.fetchImage tc (usrProfileImageURL . twUser $ tw)
        case ce of
            Just tex -> liftIO $ do
                {-
                GL.blend      GL.$= GL.Enabled
                GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                GL.blendColor GL.$= (GL.Color4 0 0 0 0.5 :: GL.Color4 GL.GLfloat)
                -}

                GL.texture         GL.Texture2D GL.$= GL.Enabled
                GL.textureBinding  GL.Texture2D GL.$= Just tex

                (w, h) <- getCurTex2DSize

                GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
                GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
                if   cw > w || ch > h
                then GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                else GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
                --GL.textureMaxAnisotropy GL.Texture2D GL.$= 8.0

                drawQuad
                    (fromIntegral cx)
                    (fromIntegral cy)
                    (fromIntegral cw)
                    (fromIntegral ch)
                    (1, 1, 1)

            _ -> liftIO $ do
                     GL.texture GL.Texture2D GL.$= GL.Disabled
                     drawQuad
                         (fromIntegral cx)
                         (fromIntegral cy)
                         (fromIntegral cw)
                         (fromIntegral ch)
                         (1, 0, 1)

-- Process all available events in both bounded and unbounded STM queues
processAllEvents :: (MonadIO m) => Either (TQueue a) (TBQueue a) -> (a -> m ()) -> m ()
processAllEvents tq processEvent = do
    me <- liftIO . atomically $ case tq of Left  tqUnbounded -> tryReadTQueue  tqUnbounded
                                           Right tqBounded   -> tryReadTBQueue tqBounded
    case me of
        Just e -> processEvent e >> processAllEvents tq processEvent
        _      -> return ()

-- Compute new layout of content rectangles for a given window size
mkUILayoutRects :: Int -> Int -> [(Int, Int, Int, Int)]
mkUILayoutRects wndWdh wndHgt =
    map (\(x, y, w, h) -> (x, wndHgt - y - h {- Flip -}, w, h))
        $ RP.packRectangles wndWdh wndHgt 1
        $    replicate 8   (127, 127)
          ++ replicate 32  (63,  63 )
          ++ replicate 128 (31,  31 )
          ++ replicate 768 (15,  15 )

processGLFWEvent :: GLFWEvent -> AppDraw ()
processGLFWEvent ev =
    case ev of
        GLFWEventError e s -> do
           window <- asks envWindow
           liftIO $ do
               traceS TLError $ "GLFW Error " ++ show e ++ " " ++ show s
               GLFW.setWindowShouldClose window True
        GLFWEventKey window k _ ks _ ->
           when (ks == GLFW.KeyState'Pressed) $ do
               when (k == GLFW.Key'Escape) $
                   liftIO $ GLFW.setWindowShouldClose window True
        GLFWEventWindowSize _ w h -> do
            -- TODO: Window resizing blocks event processing,
            --       see https://github.com/glfw/glfw/issues/1
            modify' $ \s -> s { stUILayoutRects = mkUILayoutRects w h }
            liftIO $ do setup2DOpenGL w h
                        traceS TLInfo $ printf "Window resized: %i x %i" w h

highResProfileImgURL :: B.ByteString -> B.ByteString
highResProfileImgURL url =
    let (h, t') = B.breakSubstring "_normal." url
        (_, t)  = B.breakSubstring "." t'
    in  if   not $ B.null t
        then h <> "_bigger" <> t
        else url

processSMEvent :: StreamMessage -> AppDraw ()
processSMEvent ev =
    -- TODO: Replace tweet / delete trace messages with summary stats every
    --       couple of messages
    case ev of
        SMParseError bs   -> liftIO . traceS TLError $ "\nStream Parse Error: " ++ B8.unpack bs
        SMTweet tw'       ->
            do modify' $ \s -> s { stStatTweetsReceived = stStatTweetsReceived s + 1 }
               -- Always try to fetch the higher resolution profile images
               -- TODO: Looks like a use case for lenses...
               let tw = tw' { twUser = (twUser tw')
                                  { usrProfileImageURL =
                                        highResProfileImgURL (usrProfileImageURL . twUser $ tw')
                                  }
                            }
               -- Insert tweet
               tweetLimit <- asks envTweetHistSize
               modify' $ \s -> s { stTweetByID = let sInsert = M.insert (twID tw)
                                                                        tw
                                                                        (stTweetByID s)
                                                     -- Delete oldest once we reached the limit
                                                 in  if   M.size sInsert > tweetLimit
                                                     then M.deleteMin sInsert
                                                     else sInsert
                                 }
        SMDelete _ _      -> modify' $ \s -> s { stStatDelsReceived = stStatDelsReceived s + 1 }
        SMBytesReceived b -> modify' $ \s -> s { stStatBytesRecvAPI = stStatBytesRecvAPI s + b }
        _                 -> liftIO . traceS TLInfo $ show ev -- Trace all other messages in full

withProcessStatusesAsync :: String -> RetryAPI -> AppDraw a -> AppDraw a
withProcessStatusesAsync uri' retryAPI f = do
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
    --
    -- We use some monad-control magic (http://www.yesodweb.com/book/monad-control) to stay
    -- in our AppDraw monad even though withAsync runs its inner in IO
    control $ \runC -> liftIO $ withAsync
        ( processStatuses
              uri
              oaClient
              oaCredential
              manager
              logFnMode
              smQueue
              retryAPI
        )
        $ \_ -> runC f

getCurTick :: IO Double
getCurTick = do
    tickUCT <- getCurrentTime
    -- Microsecond precision, should be fine with a Double considering the
    -- number of seconds in a day
    return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)
    --
    -- TODO: Compare with GLFW timer
    --
    -- Just time <- GLFW.getTime
    -- return time

{-# NOINLINE traceStats #-}
traceStats :: AppDraw ()
traceStats = do
        time <- liftIO $ getCurTick
        -- Record frame time
        modify' $ \s -> s { stFrameTimes = BS.push_ time $ stFrameTimes s }
        -- Time to trace again?
        lastSTrace <- gets stLastStatTrace
        interval <- asks envStatTraceInterval 
        when (time - lastSTrace > interval) $ do
            modify' $ \s -> s { stLastStatTrace = time }
            ic         <- asks envImageCache
            icStats    <- liftIO $ ImageCache.gatherCacheStats ic
            tc         <- asks envTextureCache
            tcStats    <- liftIO $ TextureCache.gatherCacheStats tc
            numTweets  <- gets stStatTweetsReceived
            numDels    <- gets stStatDelsReceived
            frameTimes <- (takeWhile (\x -> time - x < interval) . BS.toList) <$> gets stFrameTimes
            apiRecv    <- gets stStatBytesRecvAPI
            let frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
                goFD prev (x:xs) = (prev - x) : goFD x xs
                goFD _    []     = []
                fdMean           = (sum frameDeltas / (fromIntegral $ length frameDeltas))
                fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
                fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
            liftIO . traceS TLInfo $ printf
                (    "Stat Trace (every %.1fsec)\n"
                  ++ "Messages Total - SMTweet: %i | SMDelete: %i | Netw. Recv.: %.3fMB\n"
                  ++ "%s\n"
                  ++ "%s\n"
                  ++ "Frametimes - "
                  ++ "Mean: %.1fFPS/%.1fms | Worst: %.1fFPS/%.1fms | Best: %.1fFPS/%.1fms "
                )
                interval
                numTweets
                numDels
                (fromIntegral apiRecv / 1024 / 1024 :: Double)
                icStats
                tcStats
                (1.0 / fdMean ) (fdMean  * 1000)
                (1.0 / fdWorst) (fdWorst * 1000)
                (1.0 / fdBest ) (fdBest  * 1000)

run :: AppDraw ()
run = do
    -- Setup OpenGL / GLFW
    window <- asks envWindow
    liftIO $ do
        (w, h) <- GLFW.getWindowSize window
        GLFW.swapInterval 1
        setup2DOpenGL w h
    -- Main loop
    let loop = do
        traceStats
        -- Stream messages
        tqSM <- asks envSMQueue
        processAllEvents (Right tqSM) processSMEvent
        -- GLFW / OpenGL
        draw
        liftIO $ {-# SCC swapAndPoll #-} do
            -- GL.finish
            GLFW.swapBuffers window
            GLFW.pollEvents
            err <- GL.get GL.errors
            unless (null err) .
                traceS TLError $ "OpenGL Error: " ++ concatMap show err
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

runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

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

main :: IO ()
main = do
    runOnAllCores -- Multicore
    -- Image cache folder
    cacheFolder <- addTrailingPathSeparator <$> getAppUserDataDirectory defImageCacheFolder
    let imgCacheFolder flagsArg = foldr
         (\f r -> case f of FlagImageCacheFolder fldr -> fldr; _ -> r) cacheFolder flagsArg
    res <- runErrorT $ do
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
              (FlagTraceEchoOn `elem` flags)
              (FlagTraceAppend `elem` flags)
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
           FlagConcImgFetches n -> fromMaybe r $ parseMaybe n
           _                    -> r)
           defConcImgFetches flags
      withSocketsDo $
        let connCount = foldr (\f r -> case f of
                FlagConKeepAlive n -> fromMaybe r $ parseMaybe n
                _                  -> r)
                defConKeepAlive flags
            timeout = foldr (\f r -> case f of
                FlagConTimeout n -> fromMaybe r $ parseMaybe n
                _                -> r)
                defConTimeout flags
        in  withManagerSettings (def { managerConnCount       = connCount
                                     , managerResponseTimeout = Just timeout
                                     }
                                ) $ \manager -> liftIO $
          let cacheSize = foldr (\f r -> case f of
                  FlagImgMemCacheSize n -> fromMaybe r $ parseMaybe n
                  _                     -> r)
                  defImgMemCacheSize flags
          in  withImageCache manager
                             (cacheSize `div` 2)
                             concImgFetches
                             (imgCacheFolder flags)
                             $ \icache -> do
            -- Event queues filled by GLFW callbacks, stream messages
            initGLFWEventsQueue <- newTQueueIO       :: IO (TQueue  GLFWEvent)
            initSMQueue         <- newTBQueueIO 1024 :: IO (TBQueue StreamMessage)
            let wndWdh = 1105
                wndHgt = 640
            withWindow wndWdh wndHgt "Twitter" initGLFWEventsQueue $ \window ->
              withTextureCache cacheSize icache $ \tcache -> do
                -- Setup reader and state for main RWS monad
                time <- getCurTick
                let envInit = Env
                        { envWindow            = window
                        , envGLFWEventsQueue   = initGLFWEventsQueue
                        , envSMQueue           = initSMQueue
                        , envImageCache        = icache
                        , envTextureCache      = tcache
                        , envLogNetworkFolder  = logNetworkFolder
                        , envLogNetworkMode    = logNetworkMode
                        , envOAClient          = oaClient
                        , envOACredential      = oaCredential
                        , envManager           = manager
                        , envTweetHistSize     = foldr (\f r -> case f of
                                                          FlagTweetHistory n ->
                                                              fromMaybe r $ parseMaybe n
                                                          _ -> r)
                                                      defTweetHistory flags
                        , envStatTraceInterval = foldr (\f r -> case f of
                                                          FlagStatTraceInterval n ->
                                                              fromMaybe r $ parseMaybe n
                                                          _ -> r)
                                                      defStatTraceInterval flags
                        }
                    stateInit = State
                        { stTweetByID          = M.empty
                        , stUILayoutRects      = mkUILayoutRects wndWdh wndHgt 
                        , stFrameTimes         = -- FPS History for the stat trace interval
                                                 BS.empty
                                                     (round $ 60 * envStatTraceInterval envInit)
                        , stLastStatTrace      = time
                        , stStatTweetsReceived = 0
                        , stStatDelsReceived   = 0
                        , stStatBytesRecvAPI   = 0
                        }
                void $ evalRWST
                    ( -- Launch thread(s) for parsing status updates
                      let withPSAsync f =
                              if   FlagFirehose `elem` flags
                              then withProcessStatusesAsync
                                       twitterStatusesRandomStreamURL
                                       RetryForever
                                       f
                              else withProcessStatusesAsync
                                       twitterUserStreamURL
                                       RetryForever
                                       . withProcessStatusesAsync
                                             (twitterHomeTimeline ++ "?count=200")
                                             (RetryNTimes 5)
                                             $ f
                          -- Enter main loop
                      in  withPSAsync run
                    )
                    envInit stateInit
      traceS TLInfo "Clean Exit"

