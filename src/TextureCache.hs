
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections, LambdaCase #-}

module TextureCache ( withTextureCache
                    , TextureCache
                    , TextureCache.fetchImage
                    , TextureCache.gatherCacheStats
                      -- Wrap TextureGrid exports
                    , debugDumpGrid
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
import qualified TextureGrid as TG

-- OpenGL texture cache on top of the ImageCache module

data TextureCacheEntry = TETexture !GL.TextureObject -- 'Large' texture, stored in its own object
                       | TEGrid    !TG.GridSlot      -- 'Small' texture, TextureGrid reference
                       deriving (Eq)

data TextureCache = TextureCache
    { tcCacheEntries :: !(IORef (LBM.Map B.ByteString TextureCacheEntry))
    , tcImageCache   :: !ImageCache
    , tcTexGrid      :: !TG.TextureGrid
    }

withTextureCache :: Int -> ImageCache -> (TextureCache -> IO ()) -> IO ()
withTextureCache maxCacheEntries tcImageCache f = do
    -- TODO: Don't hardcode all these parameters, make them arguments of withTextureCache
    TG.withTextureGrid 512
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
               mapM_ (\case TETexture tex -> GL.deleteObjectName tex; _ -> return ())
                . map snd . LBM.toList $ cacheEntries
          )
          f
-- @@@
--
-- TODO: Fix this QuadUV thing, don't store the UVs in different ways all over the place,
--       don't use (0, 0, 1, 1) when we really mean QuadUVDefault, still avoid creating a
--       dependency on QuadRendering just for the QuadUV type
--
-- @@@

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
           -> IO ( Maybe ( GL.TextureObject -- Texture we inserted into
                         , Float, Float     -- UV Bottom Left
                         , Float, Float     -- UV Top Right
                         )
                 )      
fetchImage (TextureCache { .. }) tick uri = do
    cacheEntries <- readIORef tcCacheEntries
    case LBM.lookup uri cacheEntries of
        (newEntries, Just (TETexture tex)) -> do
            writeCache newEntries
            return $ resDefaultUV tex
        (newEntries, Just (TEGrid (TG.viewGridSlot -> TG.GridSlot tex _ _ u0 v0 u1 v1))) -> do
            writeCache newEntries
            return $ Just (tex, u0, v0, u1, v1)
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
                    let (newEntries, delEntry) = LBM.insert uri (TETexture tex) cacheEntries
                    case delEntry of
                        Just (_, TETexture delTex) -> GL.deleteObjectName delTex
                        Just (_, TEGrid    slot  ) -> TG.freeSlot tcTexGrid slot
                        Nothing                    -> return ()
                    -- Remove from image cache
                    deleteImage tcImageCache uri
                    -- Write back the cache directory and return
                    writeCache newEntries
                    return $ Just (tex, 0, 0, 1, 1)
                _ -> return Nothing
    where writeCache   = writeIORef tcCacheEntries
          resDefaultUV = Just . (, 0, 0, 1, 1)

gatherCacheStats :: TextureCache -> IO String
gatherCacheStats tc = do
    cache <- readIORef $ tcCacheEntries tc
    let dir = LBM.toList cache
    (mem, maxWdh, maxHgt) <-
        foldM ( \r@(mem', maxWdh', maxHgt') (_, entry) -> case entry of
                    TETexture tex -> do
                        GL.textureBinding GL.Texture2D GL.$= Just tex
                        (w, h) <- getCurTex2DSize
                        return (mem' + w * h * 4, max w maxWdh', max h maxHgt')
                    _ -> return r
              )
              (0, 0, 0)
              dir
    (numGridTex, numFreeSlots, gridTexWdh, (slotWdh, slotHgt), ifmt)
        <- TG.getGridMemoryUsage $ tcTexGrid tc
    return $ printf ( "Dir. Capacity: %i/%i · MemImg: %3.fMB · LargestImg: %ix%i | " ++
                      "GridTex: %i x %ix%ix%s · %ix%i slots (free: %i)"
                    )
                    (fst $ LBM.size cache)
                    (snd $ LBM.size cache)
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

