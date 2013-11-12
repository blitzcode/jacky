
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


data QuadPosition = QPCorners    Float Float Float Float -- X1 Y1 X2 Y2
                  | QPOriginSize Float Float Float Float -- X Y W H
                  | QPCenterSize Float Float Float Float -- X Y W H
                  deriving (Show)

type RGBA = (Float, Float, Float, Float)

data QuadColor = QCWhite
               | QCSolid RGBA
               | QCBottomTopGradient RGBA RGBA
               | QCLeftRightGradient RGBA RGBA
               deriving (Show)

data QuadTransparency = QTNone
                      | QTBlend Float
                      | QTSrcAlpha

drawQuad :: QuadPosition
         -> Float
         -> QuadColor
         -> QuadTransparency
         -> Maybe GL.TextureObject
         -> IO ()
drawQuad pos depth col trans tex = do
    let pos' = case pos of QPCorners x1 y1 x2 y2 -> [ (x1, y1), (x2, y1), (x2, y2), (x1, y2) ]
                           QPOriginSize x y w h  -> [ (x    , y    )
                                                    , (x + w, y    )
                                                    , (x + w, y + h)
                                                    , (x    , y + h)
                                                    ]
                           QPCenterSize x y w h  -> [ (x - w / 2, y - h / 2)
                                                    , (x + w / 2, y - h / 2)
                                                    , (x + w / 2, y + h / 2)
                                                    , (x - w / 2, y + h / 2)
                                                    ]
        cols = case col of QCWhite                 -> replicate 4 (1, 1, 1, 1)
                           QCSolid c               -> replicate 4 c
                           QCBottomTopGradient b t -> b : b : t : t : []
                           QCLeftRightGradient l r -> l : r : l : r : []
        texs = [ (0, 0), (1, 0), (1, 1), (0, 1) ]
    case trans of QTNone         -> GL.blend GL.$= GL.Disabled
                  QTBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= (GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat))
                  QTSrcAlpha     -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    case tex of Just _  -> do
                    GL.texture         GL.Texture2D      GL.$= GL.Enabled
                    GL.textureBinding  GL.Texture2D      GL.$= tex
                    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
                    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
                    -- TODO: Disable magnification filter if we're mapping pixels and texels
                    --       1:1. Some GPUs introduce blurriness otherwise
                    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                Nothing -> GL.texture GL.Texture2D GL.$= GL.Disabled
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity
    GL.renderPrimitive GL.Quads . forM_ (zip3 pos' cols texs) $
        \((x, y), (r, g, b, a), (u, v)) -> do
            color4f r g b a
            texCoord2f u v
            vertex3f x y (-depth)


draw :: AppDraw ()
draw = do
    window <- asks envWindow
    (w, h) <- liftIO $ (\(w, h) -> (fromIntegral w, fromIntegral h)) <$> GLFW.getWindowSize window

    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 0 0 0 0.0 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal
        drawQuad (QPOriginSize 0 0 w h)
                 100
                 (QCBottomTopGradient (0.2, 0.2, 0.2, 1) (0.4, 0.4, 1, 1))
                 QTNone
                 Nothing
        drawQuad (QPOriginSize 0 0 w 16)
                 10
                 QCWhite
                 (QTBlend 0.5)
                 Nothing
        drawQuad (QPOriginSize 0 (h - 80) w h)
                 10
                 QCWhite
                 (QTBlend 0.5)
                 Nothing

    tweets <- gets stTweetByID
    tiles  <- gets stUILayoutRects
    tc     <- asks envTextureCache

    forM_ (zip tiles (M.toDescList tweets)) $ \((cx, cy, cw, ch), (_, tw)) -> do
        ce <- liftIO $ TextureCache.fetchImage tc (usrProfileImageURL . twUser $ tw)
        case ce of
            Just tex -> liftIO $ do
                {-
                (w, h) <- getCurTex2DSize

                GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
                GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
                if   cw > w || ch > h
                then GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                else GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
                --GL.textureMaxAnisotropy GL.Texture2D GL.$= 8.0
                -}

                drawQuad (QPOriginSize (fromIntegral cx) (fromIntegral cy)
                                       (fromIntegral cw) (fromIntegral ch))
                         50
                         QCWhite
                         QTNone
                         (Just tex)

            _ -> liftIO $ do
                drawQuad (QPOriginSize (fromIntegral cx) (fromIntegral cy)
                                       (fromIntegral cw) (fromIntegral ch))
                         50
                         (QCSolid (1, 0, 1, 1))
                         QTNone
                         Nothing

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
          time <- liftIO $ getTick
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

