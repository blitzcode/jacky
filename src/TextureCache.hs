
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, LambdaCase #-}

module TextureCache ( withTextureCache
                    , TextureCache
                    , TextureCache.fetchImage
                    , TextureCache.gatherCacheStats
                      -- Wrap TextureGrid exports
                    , debugDumpGrid
                      -- Re-exports from QuadTypes
                    , QuadUV(..)
                    ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import Control.Exception
import Control.Monad
import Control.Applicative
import Text.Printf
import Data.IORef
import Data.Word

import ImageCache
import qualified LRUBoundedMap as LBM
import Trace
import GLHelpers
import qualified TextureGrid as TG
import QuadTypes

-- OpenGL texture cache on top of the ImageCache module

data TextureCacheEntry = TETexture !GL.TextureObject -- 'Large' texture, stored in its own object
                       | TEGrid    !TG.GridSlot      -- 'Small' texture, TextureGrid reference
                       deriving (Eq)

data TextureCache = TextureCache
    { tcCacheEntries   :: !(IORef (LBM.Map B.ByteString TextureCacheEntry))
    , tcImageCache     :: !ImageCache
    , tcTexGrid        :: !TG.TextureGrid
    , tcUseTextureGrid :: !Bool
    }

withTextureCache :: Int
                 -> Bool
                 -> Int
                 -> (Int, Int)
                 -> ImageCache
                 -> (TextureCache -> IO ())
                 -> IO ()
withTextureCache maxCacheEntries
                 tcUseTextureGrid
                 gridTexSize
                 (smallTexWdh, smallTexHgt)
                 tcImageCache
                 f = do
    TG.withTextureGrid gridTexSize
                       1
                       (smallTexWdh, smallTexHgt)
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
                 mapM_ (\case TETexture tex -> GL.deleteObjectName tex; _ -> return ())
                     . map snd . LBM.toList $ cacheEntries
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
fetchImage :: TextureCache
           -> Double
           -> B.ByteString
           -> IO (Maybe (GL.TextureObject, QuadUV))
fetchImage (TextureCache { .. }) tick uri = do
    -- TODO: Add some exception safety to this function. It's somewhat tricky, as there
    --       are several data structures being potentially updated (TextureCache, ImageCache
    --       and TextureGrid) as well as resources being allocated
    --
    -- TODO: Might need to limit amount of texture uploads per-frame. We could simply return
    --       Nothing after a certain amount of ms or MB
    cacheEntries <- readIORef tcCacheEntries
    case LBM.lookup uri cacheEntries of
        -- Cache hit, texture
        (newEntries, Just (TETexture tex)) -> do
            writeCache newEntries
            return $ Just (tex, QuadUVDefault)
        -- Cache hit, grid slot
        (newEntries, Just (TEGrid (TG.viewGridSlot -> TG.GridSlot tex _ _ uv))) -> do
            writeCache newEntries
            return $ Just (tex, uv)
        -- Cache miss
        (_, Nothing ) -> do
            hicFetch <- ImageCache.fetchImage tcImageCache tick uri
            case hicFetch of
                Just (Fetched (ImageRes w h img)) -> do
                    -- Image cache hit. Small enough to insert into grid, or do we need to
                    -- allocate a texture?
                    entry <- if   tcUseTextureGrid && TG.isGridSized tcTexGrid w h
                             then TEGrid    <$> TG.insertImage tcTexGrid w h img
                             else TETexture <$> newTexture2D GL.RGBA
                                                             GL.RGBA8
                                                             GL.UnsignedByte
                                                             (w, h)
                                                             (TCUpload img)
                                                             True
                                                             (Just TFMinMag)
                                                             True
                    -- Insert into cache, delete any overflow
                    let (newEntries, delEntry) = LBM.insert uri entry cacheEntries
                    case delEntry of
                        Just (_, TETexture delTex) -> GL.deleteObjectName delTex
                        Just (_, TEGrid    slot  ) -> TG.freeSlot tcTexGrid slot
                        Nothing                    -> return ()
                    -- Remove from image cache
                    deleteImage tcImageCache uri
                    -- Write back the cache directory and return
                    writeCache newEntries
                    return $ case entry of
                        TETexture tex -> Just (tex, QuadUVDefault)
                        TEGrid (TG.viewGridSlot -> TG.GridSlot tex _ _ uv) -> Just (tex, uv)
                _ -> return Nothing
    where writeCache = writeIORef tcCacheEntries

gatherCacheStats :: TextureCache -> IO String
gatherCacheStats tc = do
    cache <- readIORef $ tcCacheEntries tc
    let dir    = LBM.toList cache
        dirLen = fst $ LBM.size cache
    (mem, maxWdh, maxHgt, texCnt) <-
        foldM ( \r@(mem', maxWdh', maxHgt', texCnt') (_, entry) -> case entry of
                    TETexture tex -> do
                        GL.textureBinding GL.Texture2D GL.$= Just tex
                        (w, h) <- getCurTex2DSize
                        return (mem' + w * h * 4, max w maxWdh', max h maxHgt', texCnt' + 1)
                    _ -> return r
              )
              (0, 0, 0, 0)
              dir
    (numGridTex, numFreeSlots, gridTexWdh, (slotWdh, slotHgt), ifmt)
        <- TG.getGridMemoryUsage $ tcTexGrid tc
    return $ printf ( "Dir. Capacity: %i/%i (%.1f%% slotrefs) · MemImg: %3.fMB" ++
                      " · LargestImg: %ix%i | GridTex: %i x %ix%ix%s · %ix%i slots (free: %i)"
                    )
                    (fst $ LBM.size cache)
                    (snd $ LBM.size cache)
                    (fromIntegral ((dirLen - texCnt) * 100) / fromIntegral dirLen :: Float)
                    (fromIntegral mem / 1024 / 1024 :: Double)
                    maxWdh
                    maxHgt
                    numGridTex
                    gridTexWdh gridTexWdh
                    (show ifmt)
                    slotWdh
                    slotHgt
                    numFreeSlots

debugDumpGrid :: TextureCache -> FilePath -> IO ()
debugDumpGrid tc = TG.debugDumpGrid (tcTexGrid tc)

