
{-# LANGUAGE PackageImports, RecordWildCards #-}

module UI ( UIState
          , UIT
          , Side(..)
          , Rectangle(..)
          , rectFromWndFB
          , rectFromXYWH
          , runUI
          , splitRect
          , split
          , frame
          , layer
          , dimensions
          , RGBA(..)
          , FillColor(..)
          , FillTransparency(..)
          , fill
          ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State.Lazy -- TODO: Use Strict/Lazy/mtl/transformers?
import Control.Monad.IO.Class
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Data.Tuple

import GLHelpers
import StateModify

-- Drawing, layout and event handling for the user interface

-- TODO: Consider FRP library like netwire or reactive-banana for UI animations

-- TODO: Use 'linear' package for OpenGL vector / matrix stuff

-- TODO: Replace immediate mode drawing with a rendering manager, storing
--       geometry in vertex buffers, batching up draw calls, sorting by texture
--       and state change etc.

-- TODO: We have a lot of overhead by running inside of a StateT on top of a
--       RWST from App. Also see here:
--
--       http://www.haskell.org/haskellwiki/Performance/Monads
--       http://www.haskell.org/pipermail/haskell-cafe/2011-September/095622.html

-- TODO: Think about having XYWH vs X1Y1X2Y2 and where to use relative vs
--       absolute coordinates. Probably need to have more of the code in
--       place. Having newtype wrappers for abs/rel might avoid bugs and
--       make the API safer
data Rectangle = Rectangle { rcX1 :: {-# UNPACK #-} !Float
                           , rcY1 :: {-# UNPACK #-} !Float
                           , rcX2 :: {-# UNPACK #-} !Float
                           , rcY2 :: {-# UNPACK #-} !Float
                           } deriving (Show)

{-
newtype ARectangle = ARectangle { fromARect :: Rectangle }
newtype RRectangle = RRectangle { fromRRect :: Rectangle }
stuff1 :: ARectangle
stuff1 = ARectangle $ Rectangle 1 2 3 4
stuff2 :: RRectangle
stuff2 = RRectangle $ Rectangle 1 2 3 4
-}

-- TODO: Record hit boxes and event handlers registered during runUI
data UIState = UIState { uisRect  :: {-# UNPACK #-} !Rectangle
                       , uisDepth :: {-# UNPACK #-} !Float
                       }

type UIT m a = StateT UIState m a

rectFromWndFB :: GLFW.Window -> IO Rectangle
rectFromWndFB wnd = do
    (rcX2, rcY2) <- liftIO $ (\(w, h) -> (fromIntegral w, fromIntegral h))
                          <$> GLFW.getFramebufferSize wnd
    return $ Rectangle { rcX1 = 0, rcY1 = 0, .. }

rectFromXYWH :: Float -> Float -> Float -> Float -> Rectangle
rectFromXYWH x y w h = Rectangle x y (x + w) (y + h)

data Side = SLeft | SRight | SBottom | STop
            deriving (Show, Eq, Enum)

splitRect :: Side -> Float -> Rectangle -> (Rectangle, Rectangle)
splitRect side pos rc =
    let (Rectangle x1 y1 x2 y2) = rc
        splitV pos' =
            ( Rectangle  x1          y1         (x1 + pos')  y2
            , Rectangle (x1 + pos')  y1          x2          y2
            )
        splitH pos' =
            ( Rectangle  x1          y1          x2         (y1 + pos')
            , Rectangle  x1         (y1 + pos')  x2          y2
            )
    in  case side of
            SLeft   ->        splitV            pos
            SRight  -> swap $ splitV (x2 - x1 - pos)
            SBottom ->        splitH            pos
            STop    -> swap $ splitH (y2 - y1 - pos)

split :: (Applicative m, Monad m) => Side -> Float -> UIT m () -> UIT m () -> UIT m ()
split side pos near far = do
    (rcNear, rcFar) <- splitRect side pos <$> gets uisRect
    frameAbsolute rcNear near
    frameAbsolute rcFar  far

rectOffset :: Rectangle -> Rectangle -> Rectangle
rectOffset inner outer =
    let (Rectangle ix1 iy1 ix2 iy2) = inner
        (Rectangle ox1 oy1 _   _  ) = outer
    in  Rectangle (ox1 + ix1) (oy1 + iy1) (ox1 + ix2) (oy1 + iy2)

frame :: Monad m => Rectangle -> UIT m a -> UIT m a
frame inner f = do
    outer <- gets uisRect
    frameAbsolute (rectOffset inner outer) f

frameAbsolute :: Monad m => Rectangle -> UIT m a -> UIT m a
frameAbsolute rc = withDiscardStateT (\s -> s { uisRect = rc })

-- TODO: Make better use of the depth range. Have a system where user can
--       specify different priorities of 'front' layers, and all the children of
--       a given layer can be guaranteed to never exceed a given depth
layer :: Monad m => UIT m a -> UIT m a
layer = withDiscardStateT (\s -> s { uisDepth = uisDepth s - 1 })

-- TODO: Implement a few more combinators
--
--       center
--       layout (using RectPacker)
--       clip (two types, one clipping the rectangle, the other using glScissor)

dimensions :: Monad m => UIT m (Float, Float)
dimensions = do
    (Rectangle x1 y1 x2 y2) <- gets uisRect
    return (x2 - x1, y2 - y1)

runUI :: Monad m => Rectangle -> Float -> UIT m a -> m (a, UIState)
runUI rc depth f = do
    runStateT f UIState { uisRect  = rc
                        , uisDepth = depth
                        }

data RGBA = RGBA {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Show)

data FillColor = FCWhite
               | FCSolid !RGBA
               | FCBottomTopGradient !RGBA !RGBA
               | FCLeftRightGradient !RGBA !RGBA
               deriving (Show)

data FillTransparency = FTNone
                      | FTBlend !Float
                      | FTSrcAlpha
                      deriving (Show)

fill :: MonadIO m
     => FillColor
     -> FillTransparency
     -> Maybe GL.TextureObject
     -> UIT m ()
fill col trans tex = do
    rc    <- gets uisRect
    depth <- gets uisDepth
    liftIO $ fillDraw rc depth col trans tex

-- TODO: Avoid redundant state changes in this function
fillDraw :: Rectangle
         -> Float
         -> FillColor
         -> FillTransparency
         -> Maybe GL.TextureObject
         -> IO ()
fillDraw rc depth col trans tex = do
    let (Rectangle x1 y1 x2 y2) = rc
        pos' = [ (x1, y1), (x2, y1), (x2, y2), (x1, y2) ]
        cols = case col of FCWhite                 -> replicate 4 (RGBA 1 1 1 1)
                           FCSolid c               -> replicate 4 c
                           FCBottomTopGradient b t -> b : b : t : t : []
                           FCLeftRightGradient l r -> l : r : l : r : []
        texs = [ (0, 0), (1, 0), (1, 1), (0, 1) ]
    case trans of FTNone         -> GL.blend GL.$= GL.Disabled
                  FTBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= (GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat))
                  FTSrcAlpha     -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    case tex of Just _ -> do
                    -- TODO: Measure speedup from avoiding texture changes, then consider
                    --       packing textures. Will probably need to do that for text
                    --       rendering anyway
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
        \((x, y), (RGBA r g b a), (u, v)) -> do
            color4f r g b a
            texCoord2f u v
            vertex3f x y (-depth)

{-# INLINE fill #-}
--{-# INLINE frame #-}
--{-# INLINE frameAbsolute #-}
--{-# INLINE fillDraw #-}
--{-# INLINE rectOffset #-}
--{-# INLINE layer #-}
--{-# INLINE runUI #-}
--{-# INLINE split #-}
--{-# INLINE splitRect #-}

