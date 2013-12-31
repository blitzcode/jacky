
{-# LANGUAGE RecordWildCards, ExistentialQuantification, RankNTypes #-}

module TextureAtlas ( TextureAtlas
                    , withTextureAtlas
                    , insertImage
                    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Data.IORef
import qualified Data.Vector.Storable as VS
import Control.Applicative
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
insertImage ta@(TextureAtlas { .. }) w h img = do
    -- Check image format
    when (w * h /= VS.length img) $
        error "insertImage - Image vector size mismatch"
    when (sizeOf (img VS.! 0) /= texelSize taIFmt) $
        error "insertImage - Texel size mismatch"

{-
foldP :: (a -> b -> b) -> (a -> Bool) -> b -> [a] -> b
foldP f p acc = foldr go acc
    where go x r | p x       = f x r
                 | otherwise = r
-}

 -- scanl :: (a -> b -> a) -> a -> [b] -> [a]

{-
    span (isNothing . fst) . map (\x -> (Nothing, x)) $ textures

    -- Find room for the image
    (tex, texX, texY) <- readIORef taTextures >>= \textures ->
        -- Try to find empty block and mark it as used
        let (textures', insResult) =
                foldr (\x@(rp, tex) (xs, r) ->
                           case RP.pack w h rp of -- Try to insert
                               (rp', Just (texX, texY)) ->
                                   ( (rp', tex) : xs
                                   , Just (tex, texX, texY)
                                   )
                               (_ , Nothing) -> (x : xs, r) -- Failure, leave everything unchanged
                      ) ([], Nothing) textures
        in  case insResult of
                Just r -> -- 
                          do writeIORef taTextures textures'
                             return r
                Nothing -> -- We didn't find a texture with enough free space, prepend a new one
                           do tex <- emptyTexture ta
                              let emptyRP                 = RP.empty taTexWdh taTexWdh
                               in (rp, Just (texX, texY)) = RP.pack w h emptyRP
                              writeIORef taTextures $ (rp, tex) : textures
                              return (tex, texX, texY)
                              -}

    {-
    (textures', x, y, tex) <-
        let go (x@(rp, tex):xs) =
                case RP.pack w h rp of
                    (rp', Just (x, y)) -> return $ (rp', tex) : xs
                    (_  , Nothing    ) -> liftM2 (:) (return x) (go xs)
            go [] = do tex <- emptyTexture ta
                       go [(RP.empty taTexWdh taTexWdh, tex)]
        in  go textures
    -}


    -- Upload texture data
    -- TODO: Make upload asynchronous using PBOs
    GL.rowAlignment GL.Unpack GL.$= 1
    VS.unsafeWith img $
        GL.texSubImage2D
            GL.Texture2D
            0
            (GL.TexturePosition2D 0 0)
            (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
            . GL.PixelData taFmt taType
    return undefined -- TODO

-- TODO:  MIP-map generation (should be deferred, i.e. not every time a texture is touched)
--        Support textures larger than the atlas size (separate list)
--        Texture deletion
--        Clamping / filtering? Maybe leave that to the client?

