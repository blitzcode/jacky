
{-# LANGUAGE   RecordWildCards
             , ExistentialQuantification
             , LambdaCase
             , ViewPatterns #-}

module TextureGrid ( TextureGrid
                   , withTextureGrid
                   , insertImage
                   {-
                   , debugDumpGrid
                   , getGridMemoryUsage
                   -}
                   , freeSlot
                   , GridSlotView(..)
                   , GridSlot
                   ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import Data.IORef
import qualified Data.Vector.Storable as VS
import Control.Monad
import Control.Exception
import Foreign.Storable
import Foreign.Marshal.Array
import System.Directory
import System.FilePath
import Text.Printf

import GLHelpers

-- Pack multiple rectangular images into a set of OpenGL textures. Unlike TextureAtlas,
-- this module supports deletion of inserted images, but only packs them into a regular
-- grid instead of using more sophisticated kD tree based bin packing. It's best used to
-- pack smaller images of similar size
--
-- TODO: While deletion of an image frees up its slot in the grid, we never actually
--       reduce the number of underlying grid textures

data TextureGrid = forall texel. Storable texel => TextureGrid
    { tgBorder     :: !Int                    -- Number of border pixels around the packed images
    , tgTexWdh     :: !Int                    -- Width of the textures we're packing into
    , tgMaxImgWdh  :: !Int                    -- Maximum dimensions of images to be inserted
    , tgMaxImgHgt  :: !Int                    -- ..
    , tgFmt        :: !GL.PixelFormat         -- Texture format
    , tgIFmt       :: !GL.PixelInternalFormat -- ..
    , tgType       :: !GL.DataType            -- ..
    , tgFiltering  :: !TextureFiltering       -- Default filtering specified for the texture object
    , tgBackground :: !texel                  -- Single texel for background filling
    , tgTextures   :: !(IORef [GL.TextureObject]) -- Texture objects
    , tgFreeSlots  :: !(IORef [GridSlot])         -- List of free grid slots
    }

-- Use view patterns to prevent outside construction of GridSlots
data GridSlotView = GridSlot !GL.TextureObject
                             !Int   !Int   -- Texel XY
                             !Float !Float -- UV Bottom Left
                             !Float !Float -- UV Top Right
newtype GridSlot = GridSlotC { viewGridSlot :: GridSlotView }

withTextureGrid :: Storable texel
                => Int
                -> Int
                -> (Int, Int)
                -> GL.PixelFormat
                -> GL.PixelInternalFormat
                -> GL.DataType
                -> texel
                -> TextureFiltering
                -> (TextureGrid -> IO a)
                -> IO a
withTextureGrid tgTexWdh
                tgBorder
                (tgMaxImgWdh, tgMaxImgHgt)
                tgFmt
                tgIFmt
                tgType
                tgBackground
                tgFiltering =
    bracket
        ( do when (sizeOf tgBackground /= texelSize tgIFmt) $
                 error "withTextureGrid - Background texel / OpenGL texture format size mismatch"
             when (tgMaxImgWdh + 2 * tgBorder > tgTexWdh ||
                   tgMaxImgHgt + 2 * tgBorder > tgTexWdh) $
                 error "withTextureGrid - Image dimensions don't fit in texture"
             tgTextures  <- newIORef []
             tgFreeSlots <- newIORef []
             return TextureGrid { .. }
        )
        ( \tg -> GL.deleteObjectNames =<< readIORef (tgTextures tg) )

-- Take free slot, allocate new texture if we're out
takeFreeSlot :: TextureGrid -> IO GridSlot
takeFreeSlot (TextureGrid { .. }) = do
    (slot:freeSlots) <- readIORef tgFreeSlots >>= \case
        [] -> do tex <- newTexture2D tgFmt
                                     tgIFmt
                                     tgType
                                     (tgTexWdh, tgTexWdh)
                                     (TCFillBG tgBackground) -- TODO: Could skip that, we're 
                                                             --       clearing the slots
                                                             --       before use anyway
                                     False
                                     (Just tgFiltering)
                                     True
                 let wb = tgMaxImgWdh + tgBorder * 2
                     hb = tgMaxImgHgt + tgBorder * 2
                     tw = fromIntegral tgTexWdh
                  in return [ GridSlotC $ GridSlot
                                  tex
                                  x
                                  y
                                  (fromIntegral (x + tgBorder    ) / tw)
                                  (fromIntegral (y + tgBorder    ) / tw)
                                  (fromIntegral (x + tgBorder + tgMaxImgWdh) / tw)
                                  (fromIntegral (y + tgBorder + tgMaxImgHgt) / tw)
                            | y <- [0..(tgTexWdh `div` hb - 1)]
                            , x <- [0..(tgTexWdh `div` wb - 1)]
                            ]
        xs -> return xs
    writeIORef tgFreeSlots freeSlots
    return slot

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
    when (w > tgMaxImgWdh || h > tgMaxImgHgt) $
       error "insertImage - Image dimensions don't fit in grid slot" 
    when (w * h /= VS.length img) $
        error "insertImage - Image vector size mismatch"
    when (sizeOf (img VS.! 0) /= texelSize tgIFmt) $
        error "insertImage - Texel size mismatch"
    slot@(viewGridSlot -> GridSlot tex x y _ _ _ _) <- takeFreeSlot tg
    -- Upload texture data (TODO: Make upload asynchronous using PBOs)
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.rowAlignment GL.Unpack GL.$= 1
    -- Start by clearing the slot with the background color (TODO: Could be done faster)
    let wb = tgMaxImgWdh + tgBorder * 2
        hb = tgMaxImgHgt + tgBorder * 2
     in withArray (replicate (wb * hb) tgBackground) $
            GL.texSubImage2D
                GL.Texture2D
                0
                (GL.TexturePosition2D (fromIntegral $ x - tgBorder)
                                      (fromIntegral $ y - tgBorder))
                (GL.TextureSize2D (fromIntegral wb) (fromIntegral hb))
                . GL.PixelData tgFmt tgType
    -- Upload image
    VS.unsafeWith img $
        GL.texSubImage2D
            GL.Texture2D
            0
            (GL.TexturePosition2D (fromIntegral x) (fromIntegral y))
            (GL.TextureSize2D     (fromIntegral w) (fromIntegral h))
            . GL.PixelData tgFmt tgType
    -- Call raw API MIP-map generation function
    -- TODO: MIP-map generation should be deferred, not every time a texture is touched
    GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
    return slot

freeSlot :: TextureGrid -> GridSlot -> IO ()
freeSlot tg slot = do
    return ()

{-
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

getAtlasMemoryUsage :: TextureAtlas -> IO ( Int                    -- Number of textures
                                          , Int                    -- Dimensions
                                          , GL.PixelInternalFormat -- Format
                                          )
getAtlasMemoryUsage (TextureAtlas { .. }) =
    readIORef taTextures >>= \ts -> return (S.length ts, taTexWdh, taIFmt)
-}
