
{-# LANGUAGE PackageImports, LambdaCase #-}

module GLHelpers ( setup2DOpenGL
                 , getCurTex2DSize
                 , getGLStrings
                 , color3f
                 , color4f
                 , vertex2f
                 , vertex3f
                 , texCoord2f
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
-- import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative
import Text.Printf
import Data.Maybe

-- Various utility functions related to OpenGL

setup2DOpenGL :: Int -> Int -> IO ()
setup2DOpenGL w h = do
    GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    -- GLU.ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)
    GL.ortho 0 (fromIntegral w) 0 (fromIntegral h) 0 1000
    -- Magic number working for NV & ATI
    -- GL.translate (GL.Vector3 0.375 0.375 0.0 :: GL.Vector3 GL.GLfloat)

-- TODO: Don't query OpenGL state
getCurTex2DSize :: IO (Int, Int)
getCurTex2DSize = (\case (GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
                         <$> (GL.get $ GL.textureSize2D (Left GL.Texture2D) 0)

getGLStrings :: IO String
getGLStrings =
  printf
    "OpenGL - Vendor: %s · Renderer: %s · Version: %s · GLSL: %s · Num Extensions: %i · GLFW: %s"
    <$> GL.get GL.vendor
    <*> GL.get GL.renderer
    <*> GL.get GL.glVersion
    <*> GL.get GL.shadingLanguageVersion
    <*> (length <$> GL.get GL.glExtensions)
    <*> (fromJust <$> GLFW.getVersionString)

-- Immediate mode helpers

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

