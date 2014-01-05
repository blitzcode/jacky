
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module TextureCache ( withTextureCache
                    , TextureCache
                    , TextureCache.fetchImage
                    , TextureCache.gatherCacheStats
                    ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import Control.Exception
import Control.Monad
import Text.Printf
import Data.IORef
import Data.Word

import ImageCache
import qualified LRUBoundedMap as LBM
import Trace
import GLHelpers
import TextureGrid

-- OpenGL texture cache on top of the ImageCache module

data TextureCache = TextureCache { tcCacheEntries :: IORef (LBM.Map B.ByteString GL.TextureObject)
                                 , tcImageCache   :: ImageCache
                                 , tcTexGrid      :: TextureGrid
                                 }

withTextureCache :: Int -> ImageCache -> (TextureCache -> IO ()) -> IO ()
withTextureCache maxCacheEntries tcImageCache f = do
    -- TODO: Don't hardcode all these parameters, make them arguments of withTextureCache
    withTextureGrid 512
                    1
                    (128, 128)
                    GL.RGBA
                    GL.RGBA8
                    GL.UnsignedByte
                    (0 :: Word32)
                    TFMinMag
                    $ \tcTexGrid ->
      bracket
          ( newIORef (LBM.empty maxCacheEntries) >>= \tcCacheEntries ->
                return $ TextureCache { .. }
          )
          ( \tc -> do
               cacheEntries <- readIORef $ tcCacheEntries tc
               case LBM.valid cacheEntries of
                   Just err -> traceS TLError $ "LRUBoundedMap: TextureCache:\n" ++ err
                   Nothing  -> return ()
               -- Shutdown
               traceT TLInfo $ "Shutting down texture cache"
               GL.deleteObjectNames . map snd . LBM.toList $ cacheEntries
          )
          f

-- Fetch an image from the texture cache, or forward the request to the image
-- cache in case we don't have it. We need the tick for the image cache (keep
-- track of retry times for failed fetches)
--
-- TODO: We should have a frame index that gets stored with each lookup in the
--       cache. This is to ensure that we never delete a texture queried for the
--       current frame. Just put those textures in a list and delete them on the
--       next request with a higher frame number
fetchImage :: TextureCache -> Double -> B.ByteString -> IO (Maybe GL.TextureObject)
fetchImage (TextureCache { .. }) tick uri = do
    cacheEntries <- readIORef tcCacheEntries
    case LBM.lookup uri cacheEntries of
        (newEntries, Just tex) -> writeIORef tcCacheEntries newEntries >> return (Just tex)
        (_,          Nothing ) -> do
            hicFetch <- ImageCache.fetchImage tcImageCache tick uri
            case hicFetch of
                -- TODO: Might need to limit amount of texture uploads per-frame. We could
                --       simply return Nothing after a certain amount of ms or MB
                Just (Fetched (ImageRes w h img)) -> {-# SCC textureUpload #-} do
                    -- TODO: Some exception safety would be nice, but even if we
                    --       wrap this into a bracketOnError there would still
                    --       be plenty of spots where an error would leak data
                    --       or leave a data structure in a bad state
                    tex <- newTexture2D GL.RGBA
                                        GL.RGBA8
                                        GL.UnsignedByte
                                        (w, h)
                                        (TCUpload img)
                                        True
                                        (Just TFMinMag)
                                        True
                    -- Insert into cache, delete any overflow
                    let (newEntries, delTex) = LBM.insert uri tex cacheEntries
                    case delTex of Just (_, obj) -> GL.deleteObjectName obj; _ -> return ()
                    -- Remove from image cache
                    deleteImage tcImageCache uri
                    -- Write back the cache directory and return
                    writeIORef tcCacheEntries newEntries
                    return $ Just tex
                _ -> return Nothing

gatherCacheStats :: TextureCache -> IO String
gatherCacheStats tc = do
    cache <- readIORef $ tcCacheEntries tc
    let dir = LBM.toList cache
    (mem, maxWdh, maxHgt) <-
        foldM ( \(mem', maxWdh', maxHgt') (_, tex) ->
                   do GL.textureBinding GL.Texture2D GL.$= Just tex
                      (w, h) <- getCurTex2DSize
                      return (mem' + w * h * 4, max w maxWdh', max h maxHgt')
              )
              (0, 0, 0)
              dir
    return $ printf "Dir. Capacity: %i/%i | Mem: %3.fMB | Largest Image: %ix%i"
                    (fst $ LBM.size cache)
                    (snd $ LBM.size cache)
                    (fromIntegral mem / 1024 / 1024 :: Double)
                    maxWdh
                    maxHgt

