
{-# LANGUAGE PackageImports, OverloadedStrings #-}

module App ( LogNetworkMode(..)
           , Env(..)
           , State(..)
           , AppDraw
           , run
           , withProcessStatusesAsync
           , mkUILayoutRects
           ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Control
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Network.URI
import Text.Printf
import System.FilePath
import GHC.Stats
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Web.Authenticate.OAuth as OA

import Trace
import Util
import Timing
import TwitterJSON
import ImageCache
import TextureCache
import ProcessStatus
import qualified RectPacker as RP
import qualified BoundedSequence as BS
import GLHelpers

-- Application logic and presentation running in AppDraw

-- TODO: Start using Lens library for records and Reader/State

-- TODO: Consider FRP library like netwire or reactive-banana for UI animations

-- TODO: Use 'linear' package for OpenGL vector / matrix stuff

-- TODO: Replace immediate mode drawing with a rendering manager, storing
--       geometry in vertex buffers, batching up draw calls, sorting by texture
--       and state change etc.

-- TODO: Draw GUI quads with depth so drawing order can be changed without
--       changing final result

-- TODO: Add a title and status bar quad, background gradient

-- TODO: Have list of UI hit boxes with associated mouse over / drag / click
--       actions. Also store depth like for drawing quads to make occluded
--       elements not steal events from foreground objects

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
    { stCurTick            :: Double
    , stTweetByID          :: M.Map Int64 Tweet
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

{-
color4f :: Float -> Float -> Float -> Float -> IO ()
color4f r g b a = GL.color $
    GL.Color4 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b) (realToFrac a)
-}

vertex2f :: Float -> Float -> IO ()
vertex2f x y = GL.vertex $ GL.Vertex2 (realToFrac x :: GL.GLfloat) (realToFrac y)

texCoord2f :: Float -> Float -> IO ()
texCoord2f u v = GL.texCoord $ GL.TexCoord2 (realToFrac u :: GL.GLfloat) (realToFrac v)

drawQuad :: Float -> Float -> Float -> Float -> (Float, Float, Float) -> IO ()
drawQuad x y w h (r, g, b) =
    GL.preservingMatrix $ do
        -- let time = 0 :: Double -- <- getCurTick
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate $
            GL.Vector3 (realToFrac $ x + w/2) (realToFrac $ y + h/2) (0.0 :: GL.GLfloat)
        -- GL.rotate (realToFrac time * 30 + (realToFrac $ x+y) :: GL.GLfloat) $ GL.Vector3 0.0 0.0 1.0
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
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

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
          ++ replicate 256 (31,  31 )
          ++ replicate 512 (15,  15 )

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
              False
              smQueue
              retryAPI
        )
        $ \_ -> runC f

{-# NOINLINE traceStats #-}
traceStats :: AppDraw ()
traceStats = do
        time       <- gets stCurTick
        -- Record frame time
        modify' $ \s -> s { stFrameTimes = BS.push_ time $ stFrameTimes s }
        -- Time to trace again?
        lastSTrace <- gets stLastStatTrace
        interval   <- asks envStatTraceInterval 
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
            gc         <- liftIO $ getGCStats
            let frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
                goFD prev (x:xs) = (prev - x) : goFD x xs
                goFD _    []     = []
                fdMean           = (sum frameDeltas / (fromIntegral $ length frameDeltas))
                fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
                fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
                bytesToMB n      = fromIntegral n / 1024.0 / 1024.0 :: Double
            liftIO . traceS TLInfo $ printf
                (    "Messages Total - SMTweet: %i | SMDelete: %i | Netw. Recv.: %.3fMB\n"
                  ++ "%s\n"
                  ++ "%s\n"
                  ++ "Frametimes - "
                  ++ "Mean: %.1fFPS/%.1fms | Worst: %.1fFPS/%.1fms | Best: %.1fFPS/%.1fms\n"
                  ++ "GC - maxUsed: %.2fMB · curUsed: %.2fMB · peakAlloc: %iMB | "
                  ++ "mutCPU: %.2fs · mutWall: %.2fs · gcCPU: %.2fs · "
                  ++ "gcWall: %.2fs · cpu: %.2fs · wall: %.2fs"
                )
                numTweets
                numDels
                (fromIntegral apiRecv / 1024 / 1024 :: Double)
                icStats
                tcStats
                (1.0 / fdMean ) (fdMean  * 1000)
                (1.0 / fdWorst) (fdWorst * 1000)
                (1.0 / fdBest ) (fdBest  * 1000)
                (bytesToMB $ maxBytesUsed           gc)
                (bytesToMB $ currentBytesUsed       gc)
                (            peakMegabytesAllocated gc)
                (mutatorCpuSeconds  gc)
                (mutatorWallSeconds gc)
                (gcCpuSeconds       gc)
                (gcWallSeconds      gc)
                (cpuSeconds         gc)
                (wallSeconds        gc)

run :: AppDraw ()
run = do
    -- Setup OpenGL / GLFW
    --
    -- TODO: glSwapInterval and glFinish just don't seem to work properly on OS X,
    --       or maybe it's an issue with GLFW / Haskell OpenGL, not sure
    window <- asks envWindow
    liftIO $ do
        (w, h) <- GLFW.getWindowSize window
        -- GLFW.swapInterval 1
        setup2DOpenGL w h
    -- Main loop
    let loop = do
          time <- liftIO $ getCurTick
          modify' $ \s -> s { stCurTick = time }
          traceStats
          -- Stream messages
          tqSM <- asks envSMQueue
          processAllEvents (Right tqSM) processSMEvent
          -- GLFW / OpenGL
          draw
          liftIO $ {-# SCC swapAndPoll #-} do
              -- GL.flush
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

