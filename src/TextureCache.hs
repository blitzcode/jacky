
{-# LANGUAGE OverloadedStrings #-}

module TextureCache ( withTextureCache
                    , TextureCache
                    , TextureCache.fetchImage
                    ) where

import HTTPImageCache
import qualified LRUBoundedMap as LBM
import Trace

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Control.Exception

data TextureCache = TextureCache { tcCacheEntries :: LBM.Map B.ByteString GL.TextureObject
                                 , tcImageCache   :: HTTPImageCache
                                 }

withTextureCache :: Int -> HTTPImageCache -> (TextureCache -> IO ()) -> IO ()
withTextureCache maxCacheEntries hic f = do
    bracket
        ( return $ TextureCache { tcCacheEntries = LBM.empty maxCacheEntries
                                , tcImageCache   = hic
                                }
        )
        ( \tc -> do
             case LBM.valid (tcCacheEntries tc) of
                 Just err -> traceS TLError $ "LRUBoundedMap: TextureCache: " ++ err
                 Nothing  -> return ()
             -- Shutdown
             traceT TLInfo "Shutting down texture cache"
             GL.deleteObjectNames . map snd . M.elems . fst . LBM.view . tcCacheEntries $ tc
        )
        f
-- Fetch an image from the texture cache, or forward the request to the image
-- cache in case we don't have it
fetchImage :: TextureCache -> B.ByteString -> IO (TextureCache, Maybe GL.TextureObject)
fetchImage tc url =
    case LBM.lookup url $ tcCacheEntries tc of
        (newEntries, Just tex) -> return (tc { tcCacheEntries = newEntries }, Just tex)
        (_,          Nothing ) -> do
            hicFetch <- HTTPImageCache.fetchImage (tcImageCache tc) url
            case hicFetch of
                Just (Fetched (HTTPImageRes w h img)) -> do
                    [tex] <- GL.genObjectNames 1 :: IO [GL.TextureObject]
                    GL.textureBinding GL.Texture2D GL.$= Just tex
                    VS.unsafeWith img $ \ptr -> do
                        -- TODO: This assumes NPOT / non-square texture support in
                        --       combination with auto generated MIP-maps
                        GL.texImage2D
                            Nothing
                            GL.NoProxy
                            0
                            GL.RGBA8
                            (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
                            0
                            (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                        -- GLU.build2DMipmaps
                        --     GL.Texture2D
                        --     GL.RGBA'
                        --     (fromIntegral w)
                        --     (fromIntegral h)
                        --     (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                    -- Call raw API MIP-map generation function, could also use
                    -- GL.generateMipmap GL.Texture2D  GL.$= GL.Enabled instead
                    GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
                    -- Insert into cache, delete any overflow
                    let (newEntries, delTex) = LBM.insertUnsafe url tex $ tcCacheEntries tc
                    case delTex of Just (_, obj) -> GL.deleteObjectNames [obj]; _ -> return ()
                    -- Remove from image cache
                    deleteImage (tcImageCache tc) url
                    return (tc { tcCacheEntries = newEntries } , Just tex)
                _ -> return (tc, Nothing)

