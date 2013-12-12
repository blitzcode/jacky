
{-# LANGUAGE PackageImports, LambdaCase #-}

module GLHelpers ( setup2D
                 , getCurTex2DSize
                 , getGLStrings
                 , uploadTexture2D
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
-- import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Monad
import Text.Printf
import Data.Maybe
import qualified Data.Vector.Storable as VS
import Foreign.Storable

-- Various utility functions related to OpenGL

setup2D :: Int -> Int -> IO ()
setup2D w h = do
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

uploadTexture2D :: Storable pixel
                => GL.PixelFormat
                -> GL.PixelInternalFormat
                -> Int
                -> Int
                -> VS.Vector pixel
                -> Bool
                -> IO GL.TextureObject
uploadTexture2D fmt ifmt w h img genMipMap = do
    [tex] <- GL.genObjectNames 1 :: IO [GL.TextureObject]
    GL.textureBinding GL.Texture2D GL.$= Just tex
    unless (w * h  == VS.length img) $ error "ImageRes size / storage mismatch"
    VS.unsafeWith img $ \ptr -> do
        -- TODO: This assumes NPOT / non-square texture support in
        --       combination with auto generated MIP-maps
        --
        -- TODO: Make upload asynchronous using PBOs
        GL.texImage2D
            Nothing
            GL.NoProxy
            0
            ifmt
            ( GL.TextureSize2D (fromIntegral w)
                               (fromIntegral h)
            )
            0
            (GL.PixelData fmt GL.UnsignedByte ptr)
    -- Call raw API MIP-map generation function, could also use
    --
    -- GL.generateMipmap GL.Texture2D GL.$= GL.Enabled
    --
    -- or
    --
    -- GLU.build2DMipmaps
    --     GL.Texture2D
    --     GL.RGBA'
    --     (fromIntegral w)
    --     (fromIntegral h)
    --     (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    --
    when (genMipMap) $
        GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
    return tex

