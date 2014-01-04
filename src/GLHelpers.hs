
{-# LANGUAGE PackageImports, LambdaCase, ExistentialQuantification #-}

module GLHelpers ( setup2D
                 , getCurTex2DSize
                 , getGLStrings
                 , TextureContents(..)
                 , newTexture2D
                 , throwOnGLError
                 , traceOnGLError
                 , setTextureFiltering
                 , TextureFiltering(..)
                 , setTextureClampST
                 , setTransparency
                 , Transparency(..)
                 , mkBindDynamicBO
                 , disableVAOAndShaders
                 , texelSize
                 , saveTextureToPNG
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
-- import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Monad
import Control.Exception
import Text.Printf
import Data.Maybe
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified Codec.Picture as JP

import Trace

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

getErrors :: Maybe String -> IO (Maybe String)
getErrors context =
    GL.get GL.errors >>= \case
        []  -> return Nothing
        err -> return . Just $
                   "OpenGL Error" ++ maybe ": " (\c -> " (" ++ c ++ "): ") context ++ show err

throwOnGLError :: Maybe String -> IO ()
throwOnGLError context =
    getErrors context >>= \case
        Nothing  -> return ()
        Just err -> error err

traceOnGLError :: Maybe String -> IO ()
traceOnGLError context =
    getErrors context >>= \case
        Nothing  -> return ()
        Just err -> traceS TLError err

-- TODO: Don't query OpenGL state
getCurTex2DSize :: IO (Int, Int)
getCurTex2DSize = (\(GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
                         <$> (GL.get $ GL.textureSize2D GL.Texture2D 0)

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

data Transparency = TRNone
                  | TRBlend !Float
                  | TRSrcAlpha
                  deriving (Eq, Ord, Show)

setTransparency :: Transparency -> IO ()
setTransparency trans =
    case trans of TRNone         -> GL.blend GL.$= GL.Disabled
                  TRBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat)
                  TRSrcAlpha     -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

-- Create, bind and allocate Vertex / Element Array Buffer Object (VBO /  EBO)
mkBindDynamicBO :: GL.BufferTarget -> Int -> IO GL.BufferObject
mkBindDynamicBO target size = do
    bo <- GL.genObjectName
    GL.bindBuffer target GL.$= Just bo
    GL.bufferData target GL.$= ( fromIntegral size -- In bytes
                               , nullPtr
                               , GL.StreamDraw -- Dynamic
                               )
    traceOnGLError $ Just "mkBindDynamicBO"
    return bo

-- Disable vertex attribute arrays and shaders
disableVAOAndShaders :: IO ()
disableVAOAndShaders = do
    GL.bindVertexArrayObject GL.$= Nothing
    GL.currentProgram        GL.$= Nothing

data TextureFiltering = TFNone | TFMinMag | TFMinOnly | TFMagOnly

setTextureFiltering :: TextureFiltering -> IO ()
setTextureFiltering TFNone    =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing        ), GL.Nearest)
setTextureFiltering TFMinMag  =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
setTextureFiltering TFMinOnly =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
setTextureFiltering TFMagOnly =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing        ), GL.Linear')

setTextureClampST :: IO ()
setTextureClampST =
    forM_ [GL.S, GL.T] $
        \x -> GL.textureWrapMode GL.Texture2D x GL.$= (GL.Repeated, GL.ClampToEdge)

-- Number of bytes in an OpenGL pixel format
texelSize :: GL.PixelInternalFormat -> Int
texelSize ifmt = case ifmt of
    GL.Alpha'     -> 1; GL.Alpha8     -> 1;
    GL.R16        -> 2; GL.R8         -> 1;
    GL.Luminance' -> 1; GL.Luminance8 -> 1;
    GL.Intensity  -> 1; GL.Intensity8 -> 1;
    GL.RGB'       -> 3; GL.RGB8       -> 3;
    _ -> 4 -- TODO: Just assume four components / float / 32bit integer for the rest

data TextureContents = TCJustAllocate
                     | forall texel. Storable texel => TCFillBG texel
                     | forall texel. Storable texel => TCUpload (VS.Vector texel)

newTexture2D :: GL.PixelFormat
             -> GL.PixelInternalFormat
             -> GL.DataType
             -> (Int, Int)
             -> TextureContents
             -> Bool
             -> Maybe TextureFiltering
             -> Bool
             -> IO GL.TextureObject
newTexture2D fmt ifmt dtype (w, h) tc genMipMap tf clamp = do
  bracketOnError
    ( GL.genObjectName )
    ( GL.deleteObjectName )
    $ \tex -> do
        GL.textureBinding GL.Texture2D GL.$= Just tex
        -- Might not conform to the default 32 bit alignment
        GL.rowAlignment GL.Unpack GL.$= 1
        -- Allocate / upload
        --
        -- TODO: This assumes NPOT / non-square texture support in
        --       combination with auto generated MIP-maps
        --
        -- TODO: Make upload asynchronous using PBOs
        --
        -- TODO: Could use immutable textures through glTexStorage + glTexSubImage
        --
        let size   = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
            texImg = GL.texImage2D GL.Texture2D GL.NoProxy 0 ifmt size 0 . GL.PixelData fmt dtype
         in case tc of
                TCJustAllocate -> texImg nullPtr
                TCFillBG bg    -> do
                    -- Check texel size
                    when (sizeOf bg /= texelSize ifmt) $
                        error "newTexture2D - Background texel and OpenGL tex. spec. size mismatch"
                    withArray (replicate (w * h) bg) $ texImg
                TCUpload img   -> do
                    -- Check vector size
                    let vsize = VS.length img * sizeOf (img VS.! 0)
                     in unless (w * h * texelSize ifmt == vsize) $
                            error "newTexture2D - Image vector and OpenGL tex. spec. size mismatch"
                    VS.unsafeWith img texImg
        -- Set default texture sampler parameters
        when (isJust tf) .
            setTextureFiltering $ fromJust tf
        when clamp
            setTextureClampST
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
        when genMipMap $ GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
        return tex

saveTextureToPNG :: GL.TextureObject
                 -> GL.PixelFormat
                 -> GL.PixelInternalFormat
                 -> GL.DataType
                 -> FilePath
                 -> IO ()
saveTextureToPNG tex fmt ifmt dtype fn = do
    GL.textureBinding GL.Texture2D GL.$= Just tex
    (w, h) <- getCurTex2DSize
    GL.rowAlignment GL.Unpack GL.$= 1
    let flipImage img = JP.generateImage (\x y -> JP.pixelAt img x (h - 1 - y)) w h
    case texelSize ifmt of
        1 -> -- Assume Y8
             do img <- VSM.new $ w * h :: IO (VSM.IOVector JP.Pixel8)
                VSM.unsafeWith img $
                    GL.getTexImage GL.Texture2D 0 . GL.PixelData fmt dtype
                JP.savePngImage fn . JP.ImageY8 . flipImage . JP.Image w h =<< VS.freeze img
        4 -> -- Assume RGBA (TODO: Did not test this variant)
             do img <- VSM.new $ w * h * 4 :: IO (VSM.IOVector JP.Pixel8)
                VSM.unsafeWith img $
                    GL.getTexImage GL.Texture2D 0 . GL.PixelData fmt dtype
                JP.savePngImage fn . JP.ImageRGBA8 . flipImage . JP.Image w h =<< VS.freeze img
        _ -> return () -- Unsupported

