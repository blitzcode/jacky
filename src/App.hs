
{-# LANGUAGE PackageImports, OverloadedStrings #-}

module App ( Env(..)
           , State(..)
           , AppDraw
           , run
           , mkUILayoutRects
           ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Data.Monoid
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.Printf
import GHC.Stats
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

import Trace
import Timing
import TwitterJSON
import ImageCache
import TextureCache
import qualified RectPacker as RP
import qualified BoundedSequence as BS
import GLHelpers
import GLFWHelpers
import StateModify
import UI

-- Application logic and presentation running in AppDraw

-- TODO: Start using Lens library for records and Reader/State

-- TODO: Use criterion package to benchmark, maybe show real-time results
--       through EKG

data Env = Env
    { envWindow            :: GLFW.Window
    , envGLFWEventsQueue   :: TQueue GLFWEvent
    , envSMQueue           :: TBQueue StreamMessage
    , envImageCache        :: ImageCache
    , envTextureCache      :: TextureCache
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

-- Don't use RWST, the writer causes a slowdown
--
-- http://www.reddit.com/r/haskell/comments/1qrp8l/improving_performance_of_complex_monad/
-- http://permalink.gmane.org/gmane.comp.lang.haskell.libraries/18040
-- http://comments.gmane.org/gmane.comp.lang.haskell.libraries/18980
--
type AppDraw = StateT State (ReaderT Env IO)

draw :: AppDraw ()
draw = do
    window <- asks envWindow
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 0 0 0 0.0 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal

    rc <- liftIO $ rectFromWndFB window
    void $ runUI rc 1000 $ do
        fill (FCBottomTopGradient (RGBA 0.2 0.2 0.2 1) (RGBA 0.4 0.4 1 1))
             FTNone
             Nothing
        layer $
            split SBottom 16
                ( fill FCWhite (FTBlend 0.5) Nothing
                )
                ( split STop 100
                      ( fill FCWhite (FTBlend 0.5) Nothing
                      )
                      ( drawAvatarTiles
                        -- fill (FCLeftRightGradient (1, 0, 0, 1) (0, 1, 0, 1)) FTNone Nothing
                      )
                )

drawAvatarTiles :: UIT AppDraw ()
drawAvatarTiles = do
    -- TODO: ...
    tiles' <- lift $ gets stUILayoutRects
    dim    <- (\(w, h) -> (round w, round h)) <$> dimensions
    when (null tiles') $
        lift $ modify' $ \s -> s { stUILayoutRects = uncurry mkUILayoutRects $ dim }

    tweets <- lift $ gets stTweetByID
    tiles  <- lift $ gets stUILayoutRects
    tick   <- lift $ gets stCurTick
    tc     <- lift $ asks envTextureCache
    forM_ (zip tiles (M.toDescList tweets)) $ \((cx, cy, cw, ch), (_, tw)) -> do
        ce <- liftIO $ TextureCache.fetchImage tc tick (usrProfileImageURL . twUser $ tw)
        case ce of
            Just tex -> do
                {-
                (w, h) <- getCurTex2DSize

                GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
                GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
                if   cw > w || ch > h
                then GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                else GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
                --GL.textureMaxAnisotropy GL.Texture2D GL.$= 8.0
                -}
                frame (rectFromXYWH (fromIntegral cx) (fromIntegral cy)
                                    (fromIntegral cw) (fromIntegral ch)
                      ) $ fill FCWhite FTNone (Just tex)

            _ -> do
                frame (rectFromXYWH (fromIntegral cx) (fromIntegral cy)
                                    (fromIntegral cw) (fromIntegral ch)
                      ) $ fill (FCSolid (RGBA 1 0 1 1)) FTNone Nothing

-- Process all available events in both bounded and unbounded STM queues
processAllEvents :: (MonadIO m) => Either (TQueue a) (TBQueue a) -> (a -> m ()) -> m ()
processAllEvents tq processEvent = do
    me <- liftIO . atomically $ case tq of Left  tqUnbounded -> tryReadTQueue  tqUnbounded
                                           Right tqBounded   -> tryReadTBQueue tqBounded
    case me of
        Just e -> processEvent e >> processAllEvents tq processEvent
        _      -> return ()

-- Compute new layout of content rectangles for a given framebuffer size
mkUILayoutRects :: Int -> Int -> [(Int, Int, Int, Int)]
mkUILayoutRects fbWdh fbHgt =
    map (\(x, y, w, h) -> (x, fbHgt - y - h {- Flip -}, w, h))
        $ RP.packRectangles fbWdh fbHgt 1
        $    replicate 8   (127, 127)
          ++ replicate 32  (63,  63 )
          ++ replicate 256 (31,  31 )
          ++ replicate 591 (15,  15 )

processGLFWEvent :: GLFWEvent -> AppDraw ()
processGLFWEvent ev =
    case ev of
        GLFWEventError e s -> do
           window <- asks envWindow
           liftIO $ do
               traceS TLError $ "GLFW Error " ++ show e ++ " " ++ show s
               GLFW.setWindowShouldClose window True
        GLFWEventKey win k {- sc -} _ ks {- mk -} _ ->
           when (ks == GLFW.KeyState'Pressed) $ do
               when (k == GLFW.Key'Escape) $
                   liftIO $ GLFW.setWindowShouldClose win True
        GLFWEventWindowSize {- win -} _ w h -> do
            -- TODO: Window resizing blocks event processing,
            --       see https://github.com/glfw/glfw/issues/1
            liftIO $ traceS TLInfo $ printf "Window resized: %i x %i" w h
        GLFWEventFramebufferSize {- win -} _ w h -> do
            liftIO $ setup2D w h
        {-
        GLFWEventMouseButton win bttn st mk -> do
            return ()
        GLFWEventCursorPos win x y -> do
            return ()
        GLFWEventScroll win x y -> do
            return ()
        -}
        _ -> return ()

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
    -- TODO: glSwapInterval and glFlush/Finish just don't seem to work properly
    --       on OS X. Maybe it's an issue with GLFW / Haskell OpenGL, not sure
    window <- asks envWindow
    liftIO $ do
        (w, h) <- GLFW.getFramebufferSize window
        -- GLFW.swapInterval 1
        setup2D w h
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

