
{-# LANGUAGE PackageImports #-}

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
getCurTex2DSize = (\(GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
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
    -- <*> (show <$> GL.get GL.glExtensions)

uploadTexture2D :: Storable pixel
                => GL.PixelFormat
                -> GL.PixelInternalFormat
                -> Int
                -> Int
                -> VS.Vector pixel
                -> Bool
                -> IO GL.TextureObject
uploadTexture2D fmt ifmt w h img genMipMap = do
    -- Check vector size
    let vsize = VS.length img * sizeOf (img VS.! 0)
        pixelsize = case ifmt of
                        GL.Alpha'     -> 1; GL.Alpha8     -> 1;
                        GL.Luminance' -> 1; GL.Luminance8 -> 1;
                        GL.Intensity  -> 1; GL.Intensity8 -> 1;
                        GL.RGB'       -> 3; GL.RGB8       -> 3;
                        _ -> 4 -- TODO: Just assume four components for the rest
     in unless (w * h * pixelsize == vsize) $
            error "Image vector and OpenGL texture specification size mismatch"
    -- Generate and bind texture object
    [tex] <- GL.genObjectNames 1 :: IO [GL.TextureObject]
    GL.textureBinding GL.Texture2D GL.$= Just tex
    -- Might not conform to the default 32 bit alignment
    GL.rowAlignment GL.Unpack GL.$= 1
    -- Upload
    VS.unsafeWith img $
        -- TODO: This assumes NPOT / non-square texture support in
        --       combination with auto generated MIP-maps
        --
        -- TODO: Make upload asynchronous using PBOs
        --
        -- TODO: Could use immutable textures through glTexStorage + glTexSubImage
        GL.texImage2D
            Nothing
            GL.NoProxy
            0
            ifmt
            ( GL.TextureSize2D (fromIntegral w)
                               (fromIntegral h)
            )
            0
            . GL.PixelData fmt GL.UnsignedByte
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
    when genMipMap $
        GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
    return tex

