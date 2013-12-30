
{-# LANGUAGE RecordWildCards, ExistentialQuantification, RankNTypes #-}

module TextureAtlas ( TextureAtlas
                    , withTextureAtlas
                    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Data.IORef
import qualified Data.Vector.Storable as VS
import Control.Monad
import Control.Exception
import Foreign.Storable
import Foreign.Marshal.Array

import GLHelpers
import qualified RectPacker as RP

-- Pack multiple rectangular images into a set of OpenGL textures

data TextureAtlas = forall texel. Storable texel => TextureAtlas
    { taBorder     :: !Int -- Number of border pixels around the packed images
    , taTexWdh     :: !Int -- Width of the textures we're packing into
    , taFmt        :: !GL.PixelFormat -- Texture format
    , taIFmt       :: !GL.PixelInternalFormat
    , taType       :: !GL.DataType
    , taTextures   :: !(IORef [(RP.RectPacker, GL.TextureObject)]) -- Texture objects / layout
    , taBackground :: !texel -- Single texel for background filling, can have multiple components
    }

withTextureAtlas :: Storable texel
                 => Int
                 -> Int
                 -> GL.PixelFormat
                 -> GL.PixelInternalFormat
                 -> GL.DataType
                 -> texel
                 -> (TextureAtlas -> IO a)
                 -> IO a
withTextureAtlas taTexWdh taBorder taFmt taIFmt taType taBackground =
    bracket
        ( do when (sizeOf taBackground /= texelSize taIFmt) $
                 error "withTextureAtlas - Background texel / OpenGL texture format size mismatch"
             newIORef [] >>= \taTextures -> return TextureAtlas { .. }
        )
        ( \ta -> mapM_ (GL.deleteObjectName . snd) =<< readIORef (taTextures ta) )

-- Create a new empty texture
emptyTexture :: TextureAtlas -> IO GL.TextureObject
emptyTexture (TextureAtlas { .. }) = do
    -- Build texture
    tex <- GL.genObjectName
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.rowAlignment GL.Unpack GL.$= 1
    -- Fill with background color
    withArray (replicate (taTexWdh * taTexWdh) taBackground) $
        -- TODO: Use immutable texture through glTexStorage
        GL.texImage2D
            GL.Texture2D
            GL.NoProxy
            0
            taIFmt
            ( let w = fromIntegral taTexWdh in GL.TextureSize2D w w )
            0
            . GL.PixelData taFmt taType
    return tex

-- Find / allocate an empty block inside one of the atlas textures and fill it with the
-- passed image data
insertImage :: Storable texel
            => TextureAtlas
            -> Int
            -> Int
            -- TODO: Maybe just use 'Ptr texel'? Less safety, but there's no real reason
            --       the data needs to be in a vector, plus we can't use multi-component
            --       texels right now (3x Float, etc.)
            -> VS.Vector texel
            -> IO ( GL.TextureObject -- Texture we inserted into
                  , Float, Float     -- UV Bottom Left
                  , Float, Float     -- UV Top Right
                  )
insertImage (TextureAtlas { .. }) w h img = do
    -- Check image format
    when (w * h /= VS.length img) $
        error "insertImage - Image vector size mismatch"
    when (sizeOf (img VS.! 0) /= texelSize taIFmt) $
        error "insertImage - Texel size mismatch"
    -- Find empty block
    -- TODO
    -- Upload texture data
    -- TODO: Make upload asynchronous using PBOs
    VS.unsafeWith img $
        GL.texSubImage2D
            GL.Texture2D
            0
            (GL.TexturePosition2D 0 0)
            (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
            . GL.PixelData taFmt taType
    return undefined -- TODO

-- TODO:  MIP-map generation
--        Support textures larger than the atlas size
--        Texture deletion

