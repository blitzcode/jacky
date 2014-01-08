
{-# LANGUAGE   RecordWildCards
             , LambdaCase
             , BangPatterns
             , ViewPatterns #-}

module TextureGrid ( TextureGrid
                   , withTextureGrid
                   , insertImage
                   , debugDumpGrid
                   , getGridMemoryUsage
                   , freeSlot
                   , isGridSized
                   , GridSlotView(..)
                   , GridSlot
                   , viewGridSlot
                     -- Re-exports from QuadTypes
                   , QuadUV(..)
                   ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import Data.IORef
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Monad
import Control.Exception
import Control.Applicative
import Foreign.Storable
import System.Directory
import System.FilePath
import Text.Printf

import GLHelpers
import Trace
import QuadTypes (QuadUV(..))

-- Pack multiple rectangular images into a set of OpenGL textures. Unlike TextureAtlas,
-- this module supports deletion of inserted images, but only packs them into a regular
-- grid instead of using more sophisticated kD tree based bin packing. It's best used to
-- pack smaller images of similar size
--
-- TODO: While deletion of an image frees up its slot in the grid, we never actually
--       reduce the number of underlying grid textures

data TextureGrid = TextureGrid
    { tgBorder     :: !Int                    -- Number of border pixels around the packed images
    , tgTexWdh     :: !Int                    -- Width of the textures we're packing into
    , tgMaxImgWdh  :: !Int                    -- Maximum dimensions of images to be inserted
    , tgMaxImgHgt  :: !Int                    -- ..
    , tgFmt        :: !GL.PixelFormat         -- Texture format
    , tgIFmt       :: !GL.PixelInternalFormat -- ..
    , tgType       :: !GL.DataType            -- ..
    , tgFiltering  :: !TextureFiltering       -- Default filtering specified for the texture object
    , tgTextures   :: !(IORef [GL.TextureObject]) -- Texture objects
    , tgFreeSlots  :: !(IORef [GridSlot])         -- List of free grid slots
    }

-- Use view patterns to prevent outside construction of GridSlots
data GridSlotView = GridSlot !GL.TextureObject
                             !Int !Int -- Texel XY
                             !QuadUV
                    deriving (Eq)
newtype GridSlot = GridSlotC { viewGridSlot :: GridSlotView }
                   deriving (Eq)

withTextureGrid :: Int
                -> Int
                -> (Int, Int)
                -> GL.PixelFormat
                -> GL.PixelInternalFormat
                -> GL.DataType
                -> TextureFiltering
                -> (TextureGrid -> IO a)
                -> IO a
withTextureGrid tgTexWdh
                tgBorder
                (tgMaxImgWdh, tgMaxImgHgt)
                tgFmt
                tgIFmt
                tgType
                tgFiltering =
    bracket
        ( do when (tgMaxImgWdh + 2 * tgBorder > tgTexWdh ||
                   tgMaxImgHgt + 2 * tgBorder > tgTexWdh) $ traceAndThrow
                 "withTextureGrid - Image dimensions don't fit in texture"
             tgTextures  <- newIORef []
             tgFreeSlots <- newIORef []
             return TextureGrid { .. }
        )
        ( \tg -> GL.deleteObjectNames =<< readIORef (tgTextures tg) )

-- Take free slot, allocate new texture if we're out
takeFreeSlot :: TextureGrid -> IO GridSlot
takeFreeSlot (TextureGrid { .. }) = do
    (slot:freeSlots) <- readIORef tgFreeSlots >>= \case
        [] -> do -- Allocate new texture, insert into list
                 tex <- newTexture2D tgFmt
                                     tgIFmt
                                     tgType
                                     (tgTexWdh, tgTexWdh)
                                     TCJustAllocate -- We're clearing the slots before use anyway
                                     False
                                     (Just tgFiltering)
                                     True
                 modifyIORef' tgTextures (tex :)
                 -- Compute slot list for the new texture
                 let wb = tgMaxImgWdh + tgBorder * 2
                     hb = tgMaxImgHgt + tgBorder * 2
                  in return [ let xwb = x * wb + tgBorder
                                  yhb = y * hb + tgBorder
                              in  GridSlotC $ GridSlot
                                      tex
                                      xwb
                                      yhb
                                      QuadUVDefault -- We compute UVs once we know the size
                                                    -- of the image stored
                            | y <- [0..(tgTexWdh `div` hb - 1)]
                            , x <- [0..(tgTexWdh `div` wb - 1)]
                            ]
        xs -> return xs
    writeIORef tgFreeSlots freeSlots
    return slot

computeSlotUV :: TextureGrid -> Int -> Int -> GridSlot -> GridSlot
computeSlotUV tg w h (viewGridSlot -> GridSlot tex x y _) =
    GridSlotC . GridSlot tex x y $
        let tw = fromIntegral $ tgTexWdh tg
        in  QuadUV (fromIntegral  x      / tw)
                   (fromIntegral  y      / tw)
                   (fromIntegral (x + w) / tw)
                   (fromIntegral (y + h) / tw)

isGridSized :: TextureGrid -> Int -> Int -> Bool
isGridSized (TextureGrid { .. }) w h | (w > tgMaxImgWdh) || (h > tgMaxImgHgt) = False
                                     | otherwise                              = True

-- Find / allocate an empty block inside one of the atlas textures and fill it with the
-- passed image data
insertImage :: Storable texel
            => TextureGrid
            -> Int
            -> Int
            -- TODO: Maybe just use 'Ptr texel'? Less safety, but there's no real reason
            --       the data needs to be in a vector, plus we can't use multi-component
            --       texels right now (3x Float, etc.)
            -> VS.Vector texel
            -> IO GridSlot
insertImage tg@(TextureGrid { .. }) w h img = do
    -- Check parameters and take a free slot
    when (not $ isGridSized tg w h) $
        traceAndThrow "insertImage - Image dimensions don't fit in grid slot"
    when (w * h /= VS.length img) $
        traceAndThrow "insertImage - Image vector size mismatch"
    when (sizeOf (img VS.! 0) /= texelSize tgIFmt) $
        traceAndThrow "insertImage - Texel size mismatch"
    slot@(viewGridSlot -> GridSlot tex slotX slotY _) <- computeSlotUV tg w h <$> takeFreeSlot tg
    -- Upload texture data (TODO: Make upload asynchronous using PBOs)
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.rowAlignment GL.Unpack GL.$= 1
    -- Copy the image inside a slot + border sized container and fill the surrounding
    -- pixels by extruding the image, hopefully reducing bleeding artefacts
    --
    -- TODO: There are better ways to do this. We could just use texture arrays
    --       (EXT_texture_array) or take care to generate our MIP-maps without bleeding
    --       and do the rest (UV wrapping, etc.) inside the shader. We'd also need to
    --       align grid slots at powers-of-two for full MIP-mapping quality
    --
    --       References:
    --
    --       http://0fps.wordpress.com/2013/07/09/texture-atlases-wrapping-and-mip-mapping/
    --       http://http.download.nvidia.com/developer/NVTextureSuite/Atlas_Tools/
    --           Texture_Atlas_Whitepaper.pdf
    --
    let wb = tgMaxImgWdh + tgBorder * 2
        hb = tgMaxImgHgt + tgBorder * 2
    upload <- VSM.new $ wb * hb
    forM_ [ (x, y)
          | y <- [(-tgBorder)..(tgMaxImgHgt + tgBorder - 1)]
          , x <- [(-tgBorder)..(tgMaxImgWdh + tgBorder - 1)]
          ]
          $ \(x, y) -> let !dstIdx = (x + tgBorder) + (y + tgBorder) * wb
                           !clampX = min (w - 1) (max 0 x)
                           !clampY = min (h - 1) (max 0 y)
                           !srcIdx = clampX + clampY * w
                       in  VSM.unsafeWrite upload dstIdx $ VS.unsafeIndex img srcIdx
    -- Upload
    VSM.unsafeWith upload $
        GL.texSubImage2D
            GL.Texture2D
            0
            (GL.TexturePosition2D (fromIntegral $ slotX - tgBorder)
                                  (fromIntegral $ slotY - tgBorder))
            (GL.TextureSize2D (fromIntegral wb) (fromIntegral hb))
            . GL.PixelData tgFmt tgType
    -- Call raw API MIP-map generation function
    -- TODO: MIP-map generation should be deferred, not every time a texture is touched
    GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
    return slot

-- TODO: Check that we're not freeing the same slot multiple times
freeSlot :: TextureGrid -> GridSlot -> IO ()
freeSlot tg slot = modifyIORef' (tgFreeSlots tg) (slot :)

debugDumpGrid :: TextureGrid -> FilePath -> IO ()
debugDumpGrid (TextureGrid { .. }) dir =
    doesDirectoryExist dir >>= \exists -> when exists $
        readIORef tgTextures >>= \textures ->
            forM_ (textures `zip` ([1..] :: [Int])) $ \(tex, i) -> do
                saveTextureToPNG
                    tex
                    tgFmt
                    tgIFmt
                    tgType
                    $ dir </> (printf "texture_grid_%i_of_%i.png" i $ length textures)

getGridMemoryUsage :: TextureGrid -> IO ( Int                    -- Number of textures
                                        , Int                    -- Number of free slots
                                        , Int                    -- Texture dimensions
                                        , (Int, Int)             -- Slot dimensions
                                        , GL.PixelInternalFormat -- Format
                                        )
getGridMemoryUsage (TextureGrid { .. }) = do
    textures  <- readIORef tgTextures
    freeSlots <- readIORef tgFreeSlots
    return ( length textures
           , length freeSlots
           , tgTexWdh
           , (tgMaxImgWdh, tgMaxImgHgt)
           , tgIFmt
           )

