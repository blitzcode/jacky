
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module TextureCache ( withTextureCache
                    , TextureCache
                    , TextureCache.fetchImage
                    , TextureCache.gatherCacheStats
                    ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Control.Exception
import Control.Monad
import Text.Printf
import Data.IORef

import ImageCache
import qualified LRUBoundedMap as LBM
import Trace
import GLHelpers

data TextureCache = TextureCache { tcCacheEntries :: IORef (LBM.Map B.ByteString GL.TextureObject)
                                 , tcImageCache   :: ImageCache
                                 }

withTextureCache :: Int -> ImageCache -> (TextureCache -> IO ()) -> IO ()
withTextureCache maxCacheEntries hic f = do
    bracket
        ( newIORef (LBM.empty maxCacheEntries) >>= \tcCacheEntries ->
              return $ TextureCache { tcImageCache = hic, .. }
        )
        ( \tc -> do
             cacheEntries <- readIORef $ tcCacheEntries tc
             case LBM.valid cacheEntries of
                 Just err -> traceS TLError $ "LRUBoundedMap: TextureCache:\n" ++ err
                 Nothing  -> return ()
             -- Shutdown
             traceT TLInfo $ "Shutting down texture cache"
             GL.deleteObjectNames . map snd . M.elems . fst . LBM.view $ cacheEntries
        )
        f

-- Fetch an image from the texture cache, or forward the request to the image
-- cache in case we don't have it
fetchImage :: TextureCache -> B.ByteString -> IO (Maybe GL.TextureObject)
fetchImage tc uri = do
    cacheEntries <- readIORef $ tcCacheEntries tc
    case LBM.lookup uri cacheEntries of
        (newEntries, Just tex) -> writeIORef (tcCacheEntries tc) newEntries >> return (Just tex)
        (_,          Nothing ) -> do
            hicFetch <- ImageCache.fetchImage (tcImageCache tc) uri
            case hicFetch of
                Just (Fetched (ImageRes w h img)) -> {-# SCC textureUpload #-} do
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
                    -- 'GL.generateMipmap GL.Texture2D GL.$= GL.Enabled' instead
                    GLR.glGenerateMipmap GLR.gl_TEXTURE_2D
                    -- Insert into cache, delete any overflow
                    let (newEntries, delTex) = LBM.insertUnsafe uri tex cacheEntries
                    case delTex of Just (_, obj) -> GL.deleteObjectNames [obj]; _ -> return ()
                    -- Remove from image cache
                    deleteImage (tcImageCache tc) uri
                    -- Write back the cache directory and return
                    writeIORef (tcCacheEntries tc) newEntries
                    return $ Just tex
                _ -> return Nothing

gatherCacheStats :: TextureCache -> IO String
gatherCacheStats tc = do
    cache <- readIORef $ tcCacheEntries tc 
    let dir = M.elems . fst $ LBM.view cache
    (mem, maxWdh, maxHgt) <-
        foldM (\(mem', maxWdh', maxHgt') (_, tex) ->
                  do GL.textureBinding GL.Texture2D GL.$= Just tex
                     (w, h) <- getCurTex2DSize
                     return (mem' + w * h * 4, max w maxWdh', max h maxHgt')
              )
              (0, 0, 0)
              dir
    return $ printf "Texture Cache - Dir. Capacity: %i/%i | Mem: %3.fMB | Largest Image: %ix%i"
                    (fst $ LBM.size cache)
                    (snd $ LBM.size cache)
                    (fromIntegral mem / 1024 / 1024 :: Double)
                    maxWdh
                    maxHgt

