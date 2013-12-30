
{-# LANGUAGE BangPatterns, PackageImports, RecordWildCards #-}

module UI ( UIState
          , UIT
          , Side(..)
          , Rectangle(..)
          , rectFromWndFB
          , rectFromXYWH
          , runUI
          , splitRect
          , split
          , center
          , frame
          , layer
          , dimensions
          , fill
          , text
            -- Re-export from QuadRendering
          , RGBA(..)
          , FillColor(..)
          , Transparency(..)
          ) where

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Data.Tuple

import QuadRendering
--import StateModify
import FontRendering

-- Drawing, layout and event handling for the user interface

-- TODO: Consider FRP library like netwire or reactive-banana for UI animations

-- TODO: Use 'linear' package for OpenGL vector / matrix stuff
--       https://github.com/ocharles/blog/blob/master/code/2013-12-02-linear-example.hs

-- TODO: We have a lot of overhead by running inside of a StateT on top of a
--       RWST from App. Also see here:
--
--       http://www.haskell.org/haskellwiki/Performance/Monads
--       http://www.haskell.org/pipermail/haskell-cafe/2011-September/095622.html

-- TODO: Think about having XYWH vs X1Y1X2Y2 and where to use relative vs
--       absolute coordinates. Probably need to have more of the code in
--       place. Having newtype wrappers for abs/rel might avoid bugs and
--       make the API safer
--
-- TODO: Can we reuse or cache the UI structure build up instead of completely redoing it
--       every frame?
data Rectangle = Rectangle { rcX1 :: !Float
                           , rcY1 :: !Float
                           , rcX2 :: !Float
                           , rcY2 :: !Float
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
data UIState = UIState { uisRect  :: !Rectangle
                       , uisDepth :: !Float
                       , uisQB    :: !QuadRenderBuffer
                       }

type UIT m a = ReaderT UIState m a

-- TODO: Maybe we should rather put this in the GLFW module or somewhere else?
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
    (rcNear, rcFar) <- splitRect side pos <$> asks uisRect
    frameAbsolute rcNear near
    frameAbsolute rcFar  far

center :: Monad m => Float -> Float -> UIT m a -> UIT m a
center w h f = do
    (Rectangle x1 y1 x2 y2) <- asks uisRect
    let centerX = (x2 - x1) / 2
        centerY = (y2 - y1) / 2
        halfW   = w / 2
        halfH   = h / 2
    frameAbsolute (Rectangle (centerX - halfW)
                             (centerY - halfH)
                             (centerX + halfW)
                             (centerY + halfH)
                  )
                  f

rectOffset :: Rectangle -> Rectangle -> Rectangle
rectOffset inner outer =
    let (Rectangle ix1 iy1 ix2 iy2) = inner
        (Rectangle ox1 oy1 _   _  ) = outer
    in  Rectangle (ox1 + ix1) (oy1 + iy1) (ox1 + ix2) (oy1 + iy2)

frame :: Monad m => Rectangle -> UIT m a -> UIT m a
frame inner f = do
    outer <- asks uisRect
    frameAbsolute (rectOffset inner outer) f

frameAbsolute :: Monad m => Rectangle -> UIT m a -> UIT m a
frameAbsolute rc = local (\s -> s { uisRect = rc })

-- TODO: Make better use of the depth range. Have a system where user can
--       specify different priorities of 'front' layers, and all the children of
--       a given layer can be guaranteed to never exceed a given depth
layer :: Monad m => UIT m a -> UIT m a
layer = local (\s -> s { uisDepth = uisDepth s - 1 })

-- TODO: Implement a few more combinators
--
--       layout (using RectPacker)
--       clip (two types, one clipping the rectangle, the other using glScissor)

dimensions :: Monad m => UIT m (Float, Float)
dimensions = do
    (Rectangle x1 y1 x2 y2) <- asks uisRect
    return (x2 - x1, y2 - y1)

runUI :: Monad m => Rectangle -> Float -> QuadRenderBuffer -> UIT m a -> m a {-(a, UIState)-}
runUI uisRect uisDepth uisQB f =
    runReaderT f UIState { .. }

fill :: MonadIO m
     => FillColor
     -> Transparency
     -> Maybe GL.TextureObject
     -> UIT m ()
fill col trans tex = do
    UIState { .. } <- ask
    liftIO $
        --drawQuadAdHocVBOShader
        --drawQuadImmediate
        drawQuad uisQB
                 (rcX1 uisRect)
                 (rcY1 uisRect)
                 (rcX2 uisRect)
                 (rcY2 uisRect)
                 uisDepth
                 col
                 trans
                 tex
                 QuadUVDefault

text :: MonadIO m
     => FontRenderer -- TODO: Keep font renderer inside the UI state?
     -> Typeface
     -> String
     -> UIT m ()
text fr face string = do
    (Rectangle x1 y1 _ _) <- asks uisRect
    qb                    <- asks uisQB
    liftIO $ drawText fr qb (round x1) (round y1) face string

{-# INLINEABLE fill #-}
--{-# INLINEABLE text #-}
--{-# INLINEABLE frame #-}
--{-# INLINEABLE frameAbsolute #-}
--{-# INLINEABLE rectOffset #-}
--{-# INLINEABLE layer #-}
--{-# INLINEABLE runUI #-}
--{-# INLINEABLE split #-}
--{-# INLINEABLE splitRect #-}

