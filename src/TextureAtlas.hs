
{-# LANGUAGE RecordWildCards, ExistentialQuantification, RankNTypes #-}

module TextureAtlas ( TextureAtlas
                    , withTextureAtlas
                    , insertImage
                    , debugDumpAtlas
                    ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import Data.IORef
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector.Storable as VS
import Control.Monad
import Control.Exception
import Foreign.Storable
import Foreign.Marshal.Array
import System.Directory
import System.FilePath
import Text.Printf

import GLHelpers
import qualified RectPacker as RP

-- Pack multiple rectangular images into a set of OpenGL textures
--
-- TODO: Support textures larger than the atlas size (separate list)
--       Texture deletion

data TextureAtlas = forall texel. Storable texel => TextureAtlas
    { taBorder     :: !Int                    -- Number of border pixels around the packed images
    , taTexWdh     :: !Int                    -- Width of the textures we're packing into
    , taFmt        :: !GL.PixelFormat         -- Texture format
    , taIFmt       :: !GL.PixelInternalFormat -- ..
    , taType       :: !GL.DataType            -- ..
    , taFiltering  :: !TextureFiltering       -- Default filtering specified for the texture object
    , taBackground :: !texel                  -- Single texel for background filling
    , taTextures   :: !(IORef (S.Seq (RP.RectPacker, GL.TextureObject))) -- Texture objs. / layout
    }

withTextureAtlas :: Storable texel
                 => Int
                 -> Int
                 -> GL.PixelFormat
                 -> GL.PixelInternalFormat
                 -> GL.DataType
                 -> texel
                 -> TextureFiltering
                 -> (TextureAtlas -> IO a)
                 -> IO a
withTextureAtlas taTexWdh taBorder taFmt taIFmt taType taBackground taFiltering =
    bracket
        ( do when (sizeOf taBackground /= texelSize taIFmt) $
                 error "withTextureAtlas - Background texel / OpenGL texture format size mismatch"
             newIORef S.empty >>= \taTextures -> return TextureAtlas { .. }
        )
        ( \ta -> F.mapM_ (GL.deleteObjectName . snd) =<< readIORef (taTextures ta) )

-- Create a new empty texture
emptyTexture :: TextureAtlas -> IO GL.TextureObject
emptyTexture (TextureAtlas { .. }) = do
    -- Build texture, configure default sampler
    tex <- GL.genObjectName
    GL.textureBinding GL.Texture2D GL.$= Just tex
    setTextureClampST
    setTextureFiltering taFiltering
    -- Fill with background color
    GL.rowAlignment GL.Unpack GL.$= 1
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
insertImage ta@(TextureAtlas { .. }) w h img = do
    -- Check image format
    when (w * h /= VS.length img) $
        error "insertImage - Image vector size mismatch"
    when (sizeOf (img VS.! 0) /= texelSize taIFmt) $
        error "insertImage - Texel size mismatch"
    -- Find room for the image. This is somewhat cumbersome and slow O(n), but we should
    -- have a manageable number of atlas textures and insertion should generally succeed
    -- quickly
    (textures', tex, texX, texY) <- readIORef taTextures >>= \textures ->
        let wb = w + taBorder * 2
            hb = h + taBorder * 2
            go (x@(rp, tex) S.:< xs) xs' = -- Try to find empty block and mark it as used
                case RP.pack wb hb rp of
                    (rp', Just (texX, texY)) -> -- Success
                      return
                        ( ( xs' S.|>      -- Rebuild sequence so far
                            (rp', tex)    -- Append updated texture atlas entry
                          ) S.>< xs       -- Stop processing remainder, just append
                        , tex, texX, texY -- Location of empty block
                        )
                    (_ , Nothing) -> -- Failure, leave everything unchanged and keep searching
                        go (S.viewl xs) (xs' S.|> x)
            go S.EmptyL _ = -- We didn't find a texture with enough free space
                            do -- Make new texture and insert
                               tex <- emptyTexture ta
                               let emptyRP                 = RP.empty taTexWdh taTexWdh
                                   (rp, Just (texX, texY)) = RP.pack wb hb emptyRP
                               return ( (rp, tex) S.<| textures -- Prepend original sequence
                                      , tex, texX, texY         -- Location of empty block
                                      )
        in  go (S.viewl textures) S.empty
    -- Write back texture atlas sequence
    -- TODO: Sort by free space, don't want to traverse a list of full textures every time
    writeIORef taTextures textures'
    -- Upload texture data
    -- TODO: Make upload asynchronous using PBOs
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.rowAlignment GL.Unpack GL.$= 1
    --
    -- TODO: Remove me (random fragments of debugging code)
    --
    -- , taStep       :: !(IORef Int)
    -- newIORef 0 >>= \taStep ->
    -- printf "(%i, %i) - %ix%i\n" (texX + taBorder) (texY + taBorder) w h
    -- i <- readIORef taStep
    -- withArray (replicate (w * h) (fromIntegral $ i * 7 :: Word8)) $
    -- saveTextureToPNG tex taFmt taIFmt taType ("step_" ++ show i ++ ".png")
    -- modifyIORef' taStep (+ 1)
    --
    VS.unsafeWith img $
        GL.texSubImage2D
            GL.Texture2D
            0
            (GL.TexturePosition2D (fromIntegral $ texX + taBorder)
                                  (fromIntegral $ texY + taBorder))
            (GL.TextureSize2D (fromIntegral w)
                              (fromIntegral h))
            . GL.PixelData taFmt taType
    -- Call raw API MIP-map generation function
    -- TODO: MIP-map generation should be deferred, not every time a texture is touched
    GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
    -- Compute UV coordinates and return
    let tw = fromIntegral taTexWdh
     in return ( tex
               , fromIntegral (texX + taBorder    ) / tw
               , fromIntegral (texY + taBorder    ) / tw
               , fromIntegral (texX + taBorder + w) / tw
               , fromIntegral (texY + taBorder + h) / tw
               )

debugDumpAtlas :: TextureAtlas -> FilePath -> IO ()
debugDumpAtlas (TextureAtlas { .. }) dir =
    doesDirectoryExist dir >>= \exists -> when exists $
        readIORef taTextures >>= \textures ->
            forM_ (F.toList textures `zip` ([1..] :: [Int])) $ \((_, tex), i) -> do
                saveTextureToPNG
                    tex
                    taFmt
                    taIFmt
                    taType
                    $ dir </> (printf "texture_atlas_%i_of_%i.png" i $ S.length textures)

