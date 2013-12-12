
module GLImmediate ( color3f
                   , color4f
                   , vertex2f
                   , vertex3f
                   , texCoord2f
                   , drawQuad
                   , RGBA(..)
                   , FillColor(..)
                   , FillTransparency(..)
                   ) where

import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL

-- Immediate mode OpenGL helper functions

color3f :: Float -> Float -> Float -> IO ()
color3f r g b = GL.color $ GL.Color3 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b)

color4f :: Float -> Float -> Float -> Float -> IO ()
color4f r g b a = GL.color $
    GL.Color4 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b) (realToFrac a)

vertex2f :: Float -> Float -> IO ()
vertex2f x y = GL.vertex $ GL.Vertex2 (realToFrac x :: GL.GLfloat) (realToFrac y)

vertex3f :: Float -> Float -> Float -> IO ()
vertex3f x y z = GL.vertex $
    GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y) (realToFrac z)

texCoord2f :: Float -> Float -> IO ()
texCoord2f u v = GL.texCoord $ GL.TexCoord2 (realToFrac u :: GL.GLfloat) (realToFrac v)

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

drawQuad :: Float
         -> Float
         -> Float
         -> Float
         -> Float
         -> FillColor
         -> FillTransparency
         -> Maybe GL.TextureObject
         -> IO ()
drawQuad x1 y1 x2 y2 depth col trans tex = do
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

