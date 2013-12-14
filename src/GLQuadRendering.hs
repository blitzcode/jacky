
module GLQuadRendering ( withGLQuadRenderer
                       , GLQuadRenderer
                       , RGBA(..)
                       , FillColor(..)
                       , FillTransparency(..)
                       , drawQuadImmediate
                       ) where

import qualified Graphics.Rendering.OpenGL as GL
-- import qualified Data.Vector.Storable as VS
import Control.Exception
import Control.Monad
-- import Control.Applicative
-- import Data.IORef

import GLImmediate

-- Module for efficient rendering of 2D quad primitives, used for UI elements and texture
-- mapped font rendering

data GLQuadRenderer = GLQuadRenderer {
                                     }

withGLQuadRenderer :: (GLQuadRenderer -> IO a) -> IO a
withGLQuadRenderer =
    bracket ( return $ GLQuadRenderer { } )
            ( \_ -> return () )

data RGBA = RGBA {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Show)

data FillColor = FCWhite
               | FCBlack
               | FCSolid !RGBA
               | FCBottomTopGradient !RGBA !RGBA
               | FCLeftRightGradient !RGBA !RGBA
               deriving (Show)

data FillTransparency = FTNone
                      | FTBlend !Float
                      | FTSrcAlpha
                      deriving (Show)

-- Simple / slow / obsolete immediate mode drawing of quads

drawQuadImmediate :: Float
         -> Float
         -> Float
         -> Float
         -> Float
         -> FillColor
         -> FillTransparency
         -> Maybe GL.TextureObject
         -> IO ()
drawQuadImmediate x1 y1 x2 y2 depth col trans tex = do
    let pos' = [ (x1, y1), (x2, y1), (x2, y2), (x1, y2) ]
        cols = case col of FCWhite                 -> replicate 4 (RGBA 1 1 1 1)
                           FCBlack                 -> replicate 4 (RGBA 0 0 0 1)
                           FCSolid c               -> replicate 4 c
                           FCBottomTopGradient b t -> [b, b, t, t]
                           FCLeftRightGradient l r -> [l, r, l, r]
        texs = [ (0, 0), (1, 0), (1, 1), (0, 1) ]
    case trans of FTNone         -> GL.blend GL.$= GL.Disabled
                  FTBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat)
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
        \((x, y), RGBA r g b a, (u, v)) -> do
            color4f r g b a
            texCoord2f u v
            vertex3f x y (-depth)

