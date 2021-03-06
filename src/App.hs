
{-# LANGUAGE PackageImports, OverloadedStrings, RecordWildCards #-}

module App ( Env(..)
           , State(..)
           , AppDraw
           , run
           , mkUILayoutRects
           ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Data.Monoid
import Data.Int
import qualified Data.Text as T
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
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
import UI
import FontRendering
import QuadRendering

-- Application logic and presentation running in AppDraw

-- TODO: Start using Lens library for records and Reader/State

-- TODO: Shouldn't the Env / State records have all strict fields?

data Env = Env
    { envWindow                  :: GLFW.Window
    , envGLFWEventsQueue         :: TQueue GLFWEvent
    , envSMQueue                 :: TBQueue StreamMessage
    , envImageCache              :: ImageCache
    , envTextureCache            :: TextureCache
    , envTweetHistSize           :: Int
    , envStatTraceInterval       :: Double
    , envDumpFT2AtlasOnTrace     :: Bool
    , envDumpTexCacheGridOnTrace :: Bool
    , envFontRenderer            :: FontRenderer
    , envQuadRenderer            :: QuadRenderer
    }

data State = State
    { stCurTick            :: Double
    , stTweetByID          :: M.Map Int64 Tweet
    , stUILayoutRects      :: [(Int, Int, Int, Int)]
      -- Statistics
    , stFrameTimes         :: BS.BoundedSequence Double
    , stLastStatTrace      :: Double
    , stLastEscPress       :: Double
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
        GL.clearColor GL.$= (GL.Color4 1 1 1 1 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal

    tweetText <- (maybe "" (\(tw, _) -> T.unpack $ twText tw) . M.maxView) <$> gets stTweetByID
    fr        <- lift $ asks envFontRenderer
    Just verdana <- liftIO $ getLoadedTypeface fr "Verdana" 12
    Just arial<- liftIO $ getLoadedTypeface fr "Arial Unicode MS" 16
    rc        <- liftIO $ rectFromWndFB window
    qr        <- asks envQuadRenderer
    (w, h) <- liftIO $ GLFW.getFramebufferSize window
    void $ withQuadRenderBuffer qr w h $ \qb ->
      runUI rc 1000 qb $ do
        fill (FCBottomTopGradient (RGBA 0.2 0.2 0.2 1) (RGBA 0.4 0.4 1 1))
             TRNone
             Nothing
             QuadUVDefault
        layer $
          split SBottom 16
            ( fill FCWhite (TRBlend 0.5) Nothing QuadUVDefault
            )
            ( split STop 100
                ( split SCenterH 0
                    ( do fill FCWhite (TRBlend 0.5) Nothing QuadUVDefault
                         center 1000 15 $
                           layer $
                             text fr arial tweetText []
                    )
                    ( do split SLeft 100
                           ( do fill (FCSolid $ RGBA 1 0 0 1) TRNone Nothing QuadUVDefault
                                text fr verdana "[TLWordWrap] Can we reuse or cache the UI structure build up?"
                                     [TLWordWrap]
                           )
                         $ split SLeft 100
                           ( do fill (FCSolid $ RGBA 0 1 0 1) TRNone Nothing QuadUVDefault
                                text fr verdana "Can we reuse or cache the UI structure build up? [TLWordWrap, TLAlignBottom] "
                                     [TLWordWrap, TLAlignBottom]
                           )
                         $ split SLeft 100
                           ( do fill (FCSolid $ RGBA 0 0 1 1) TRNone Nothing QuadUVDefault
                                text fr verdana "[TLWordWrap, TLCenterHorz] Can we reuse or cache the UI structure build up?"
                                     [TLWordWrap, TLCenterHorz]
                           )
                         $ split SLeft 100
                           ( do fill (FCSolid $ RGBA 1 0 1 1) TRNone Nothing QuadUVDefault
                                text fr verdana "Can we reuse or cache [TLWordWrap, TLCenterVert] the UI structure build up?"
                                     [TLWordWrap, TLCenterVert]
                           )
                         $ split SLeft 100
                           ( do fill (FCSolid $ RGBA 0 1 1 1) TRNone Nothing QuadUVDefault
                                text fr verdana "Can we reuse or cache [TLWordWrap, TLCenterVert, TLCenterHorz] the UI structure build up?"
                                     [TLWordWrap, TLCenterVert, TLCenterHorz]
                           )
                         $ split SLeft 100
                           ( do fill (FCSolid $ RGBA 1 1 0 1) TRNone Nothing QuadUVDefault
                                text fr verdana "[] Single\nNewline\nDouble\n\nNewline"
                                     []
                           )
                         $ split SLeft 100
                           ( do fill (FCSolid $ RGBA 1 1 1 1) TRNone Nothing QuadUVDefault
                                text fr verdana "[] Can we reuse or cache the UI structure build up?"
                                     []
                           )
                         $ return ()
                    )
                )
                ( do drawAvatarTiles
                     layer $
                       fontRenderingTest
                     center 200 100 $
                       layer $
                         fill (FCSolid $ RGBA 0 1 0 1)
                              (TRBlend 0.75)
                              Nothing
                              QuadUVDefault
                )
            )

fontRenderingTest :: UIT AppDraw ()
fontRenderingTest = do
    fr            <- lift $ asks envFontRenderer
    Just verdana  <- liftIO $ getLoadedTypeface fr "Verdana" 12
    Just futura   <- liftIO $ getLoadedTypeface fr "Futura" 24
    Just lucida   <- liftIO $ getLoadedTypeface fr "Lucida Grande" 32
    Just arial_nr <- liftIO $ getLoadedTypeface fr "Arial Narrow" 48
    Just arial    <- liftIO $ getLoadedTypeface fr "Arial Unicode MS" 16

    {-
    -- TODO: ...
    --fill FCWhite FTNone Nothing
    liftIO $ GL.texture         GL.Texture2D      GL.$= GL.Enabled
    liftIO $ do
                    [tex] <- GL.genObjectNames 1 :: IO [GL.TextureObject]
                    GL.textureBinding GL.Texture2D GL.$= Just tex
                    let v = VS.replicate 64 0xFFFFFFFF :: VS.Vector Word32
                    VS.unsafeWith v $ \ptr -> do
                        GL.texImage2D
                            Nothing
                            GL.NoProxy
                            0
                            GL.RGBA8
                            (GL.TextureSize2D 8 8)
                            0
                            (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                    GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
    -}

    split STop 40 (text fr arial_nr "1234567890 !@#$%^&*()_+ aAbBcCdDeEfFgGhHiIjJkKlL" [])
     $ split STop 40 (text fr verdana "Haskell is an advanced purely-functional programming language. An open-source product of more than twenty years of cutting-edge research, it allows rapid development of robust, concise" [])
     $ split STop 40 (text fr futura "Ä Ö Ü 漢字, Le projet d’encyclopédie libre que vous pouvez améliorer - którą każdy może redagować." [])
     $ split STop 40 (text fr lucida "Καλώς ήλθατε στη Βικιπαίδεια - Заглавная страница - la enciclopedia de" [])
     $ split STop 40 (text fr arial "العربية | Беларуская | Čeština | Ελληνικά | فارسی | 한국어 | עברית | ქართული | 日本語 | ไทย | 中文 | Українська | ᓃᔥᑕᒻᐹᔅᑌᒋᓂᑲᓐ" [])
     $ split STop 40 (text fr arial "ウィキペディアへようこそ ウィキペディアは誰でも編集できるフリー百科事典です" [])
     $ split STop 40 (text fr arial "努尔哈赤（1559年－1626年9月30日），爱新觉罗氏，出身建州左卫都指挥使世家旁系。努尔哈赤少年时曾以采参为生，常到抚顺关马市进行贸易活动。后因父" [])
     $ split STop 40 (text fr arial_nr "Text is available under the Creative Commons" [])
     $ split STop 40 (text fr lucida "Image Cache - Netw. Recv. Total: 0.000MB · Mem 0.000MB | Req: 0/256" [])
     $ split STop 40 (text fr futura "OpenGL - Vendor: NVIDIA Corporation · Renderer: NVIDIA GeForce 9400M OpenGL Engine" [])
     $ split STop 40 (text fr verdana "2116223572217043619665288&15592418065293&162624180&926376&3026085652896529229233260323527332599276636529220986365232431424030240382135537117" [])
     $ return ()

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
            Just (tex, uv) -> do
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
                      ) $ fill FCWhite TRNone (Just tex) uv
            _ -> frame (rectFromXYWH (fromIntegral cx) (fromIntegral cy)
                                     (fromIntegral cw) (fromIntegral ch)
                       ) $ fill (FCSolid (RGBA 1 0 1 1)) TRNone Nothing QuadUVDefault

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
               when (k == GLFW.Key'Escape) $ do
                   lastPress <- gets stLastEscPress
                   tick      <- gets stCurTick
                   -- Only close when ESC has been pressed twice quickly
                   when (tick - lastPress < 0.5) .
                       liftIO $ GLFW.setWindowShouldClose win True
                   modify' $ \s -> s { stLastEscPress = tick }
        GLFWEventWindowSize {- win -} _ w h -> do
            -- TODO: Window resizing blocks event processing,
            --       see https://github.com/glfw/glfw/issues/1
            liftIO $ traceS TLInfo $ printf "Window resized: %i x %i" w h
        GLFWEventFramebufferSize {- win -} _ w h -> do
            liftIO $ setupViewport w h
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
        Env   { .. } <- ask
        State { .. } <- get
        -- Record frame time
        modify' $ \s -> s { stFrameTimes = BS.push_ stCurTick stFrameTimes }
        -- Time to trace again?
        when (stCurTick - stLastStatTrace > envStatTraceInterval) $ do
            modify' $ \s -> s { stLastStatTrace = stCurTick }
            icStats        <- liftIO $ ImageCache.gatherCacheStats    envImageCache
            tcStats        <- liftIO $ TextureCache.gatherCacheStats  envTextureCache
            frStats        <- liftIO $ FontRendering.gatherCacheStats envFontRenderer
            qrStats        <- liftIO $ gatherRenderStats              envQuadRenderer
            GCStats { .. } <- liftIO $ getGCStats
            -- Debug: write textures to disk
            when envDumpFT2AtlasOnTrace     $ liftIO $ debugDumpAtlas envFontRenderer "."
            when envDumpTexCacheGridOnTrace $ liftIO $ debugDumpGrid  envTextureCache "."
            let frameTimes       = takeWhile (\x -> stCurTick - x < envStatTraceInterval) .
                                       BS.toList $ stFrameTimes
                frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
                goFD prev (x:xs) = (prev - x) : goFD x xs
                goFD _    []     = []
                fdMean           = (sum frameDeltas / (fromIntegral $ length frameDeltas))
                fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
                fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
                bytesToMB n      = fromIntegral n / 1024.0 / 1024.0 :: Double
            liftIO . traceS TLInfo $ printf
                (    "Font Rendering - %-69s"
                  ++ "Messages Total - SMTweet: %i | SMDelete: %i | Netw. Recv.: %.3fMB\n"
                  ++ "Quad Rendering - %-69sFrametimes     - "
                  ++ "Mean: %.1fFPS/%.1fms | Worst: %.1fFPS/%.1fms | Best: %.1fFPS/%.1fms\n"
                  ++ "GC             - maxUsed: %.2fMB · curUsed: %.2fMB · peakAlloc: %iMB | "
                  ++ "mutCPU: %.2fs · mutWall: %.2fs · gcCPU: %.2fs · "
                  ++ "gcWall: %.2fs · cpu: %.2fs · wall: %.2fs\n"
                  ++ "Image Cache    - %s\nTexture Cache  - %s"
                )
                frStats
                stStatTweetsReceived
                stStatDelsReceived
                (fromIntegral stStatBytesRecvAPI / 1024 / 1024 :: Double)
                qrStats
                (1.0 / fdMean ) (fdMean  * 1000)
                (1.0 / fdWorst) (fdWorst * 1000)
                (1.0 / fdBest ) (fdBest  * 1000)
                (bytesToMB $ maxBytesUsed)
                (bytesToMB $ currentBytesUsed)
                (            peakMegabytesAllocated)
                mutatorCpuSeconds
                mutatorWallSeconds
                gcCpuSeconds
                gcWallSeconds
                cpuSeconds
                wallSeconds
                icStats
                tcStats

run :: AppDraw ()
run = do
    -- Setup OpenGL / GLFW
    --
    -- TODO: glSwapInterval and glFlush/Finish just don't seem to work properly
    --       on OS X. Maybe it's an issue with GLFW / Haskell OpenGL, not sure
    window <- asks envWindow
    liftIO $ do
        (w, h) <- GLFW.getFramebufferSize window
        GLFW.swapInterval 0
        setupViewport w h
    -- Load fonts
    fr <- asks envFontRenderer
    liftIO $ do
        void $ loadTypeface fr "/Library/Fonts/Futura.ttc"                24 Nothing     Nothing
        void $ loadTypeface fr "/Library/Fonts/Arial Narrow.ttf"          48 Nothing     Nothing
        void $ loadTypeface fr "/System/Library/Fonts/LucidaGrande.ttc"   32 Nothing     Nothing
        void $ loadTypeface fr "/Library/Fonts/Microsoft/Verdana.ttf"     12 (Just True) Nothing
        void $ loadTypeface fr "/Library/Fonts/Arial Unicode.ttf"         16 Nothing     Nothing
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
              -- TODO: See if just calling it once per-frame already causes a slowdown due
              --       to synchronization
              traceOnGLError $ Just "main loop"
          tqGLFW <- asks envGLFWEventsQueue
          processAllEvents (Left tqGLFW) processGLFWEvent
          -- Done?
          close <- liftIO $ GLFW.windowShouldClose window
          unless close loop
     in loop

