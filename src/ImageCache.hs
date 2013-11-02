
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable, RankNTypes #-}

module ImageCache ( ImageCache
                  , ImageRes(..)
                  , CacheEntry(..)
                  , withImageCache
                  , fetchImage
                  , deleteImage
                  , gatherCacheStats
                  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import Data.Word
import Data.Bits
import Data.IORef
import Data.Typeable
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.URI
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Conduit
import Network.HTTP.Conduit
import System.IO.Error
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPT
import Control.Exception
import Control.Applicative
import Text.Printf

import qualified LRUBoundedMap as LBM
import Trace

-- Caching system (disk & memory) for image fetches over HTTP and from disk

-- TODO: Add support for retiring elements from the disk cache, consider not having the disk
--       cache at all and only use it to speed up application startup / offline mode

data ImageCache = ImageCache
    { icCacheFolder    :: B.ByteString
    , icOutstandingReq :: TVar (LBM.Map B.ByteString ()) -- No 'v', used as a Set + LRU
    , icCacheEntries   :: TVar (LBM.Map B.ByteString CacheEntry)
      -- Statistics
    , icBytesTrans     :: IORef Word64
    , icMisses         :: IORef Word64
    , icDiskHits       :: IORef Word64
    , icMemHits        :: IORef Word64
    }

data CacheEntry = Fetching -- We keep in-progress entries in the cache to avoid double fetches
                | Fetched !ImageRes
                | CacheError  -- Failed to load / fetch / decode image

-- TODO: Add 'retryAfterNSec' field to CacheError to deal with failed fetches / decompression
--       while allowing to try again at some point

data ImageRes = ImageRes !Int !Int !(VS.Vector Word32)

mkURICacheFn :: ImageCache -> B.ByteString -> B.ByteString
mkURICacheFn ic url = icCacheFolder ic
                      <> (B8.pack . escapeURIString isUnescapedInURIComponent $ B8.unpack url)

withImageCache :: Manager
               -> Int
               -> Int
               -> String
               -> (ImageCache -> IO ())
               -> IO ()
withImageCache manager memCacheEntryLimit numConcReq cacheFolder f = do
    -- Make sure our cache folder exists
    createDirectoryIfMissing True cacheFolder
    -- Build record 
    initOutstandingReq <- newTVarIO $ LBM.empty $ memCacheEntryLimit `div` 2
    initCacheEntries   <- newTVarIO $ LBM.empty memCacheEntryLimit
    initIORefs         <- forM ([1..4] :: [Int]) (\_ -> newIORef 0 :: IO (IORef Word64))
    let ic = ImageCache { icCacheFolder    = B8.pack $ addTrailingPathSeparator cacheFolder
                        , icOutstandingReq = initOutstandingReq
                        , icCacheEntries   = initCacheEntries
                        , icBytesTrans     = initIORefs !! 0
                        , icMisses         = initIORefs !! 1
                        , icDiskHits       = initIORefs !! 2
                        , icMemHits        = initIORefs !! 3
                        }
    bracket -- Fetch thread launch and cleanup
        --
        -- Note the asyncWithUnmask. Otherwise all fetch threads would be
        -- created with a MaskedInterruptible masking state (bracket does that
        -- for its 'before' operations) and we would have hangs during cleanup
        --
        (forM [1..numConcReq] $ \_ -> asyncWithUnmask $ fetchThread ic manager)
        (\threads -> do
            -- Error checking, statistics
            req   <- LBM.valid <$> (atomically . readTVar $ icOutstandingReq ic)
            cache <- LBM.valid <$> (atomically . readTVar $ icCacheEntries   ic)
            case req   of Just err -> traceS TLError $ "LRUBoundedMap: icOutstandingReq: " ++ err
                          Nothing  -> return ()
            case cache of Just err -> traceS TLError $ "LRUBoundedMap: icCacheEntries: "   ++ err
                          Nothing  -> return ()
            -- Shutdown
            traceT TLInfo "Shutting down image fetch threads"
            forM_ threads cancel
            forM_ threads $ \thread -> do
                r <- waitCatch thread
                case r of
                    Left ex -> traceS TLError $ printf "Exception from fetch thread '%s': %s"
                                                       (show $ asyncThreadId thread)
                                                       (show ex)
                    _       -> return ()
        )
        (\_ -> f ic)

dynImgToRGBA8 :: JP.DynamicImage -> Either String (JP.Image JP.PixelRGBA8)
dynImgToRGBA8 di =
    case di of
        JP.ImageYCbCr8 i -> Right $ JPT.promoteImage (JPT.convertImage i :: JP.Image JP.PixelRGB8)
        JP.ImageRGBA8  i -> Right $ i
        JP.ImageRGB8   i -> Right $ JPT.promoteImage i
        JP.ImageY8     i -> Right $ JPT.promoteImage i
        JP.ImageYA8    i -> Right $ JPT.promoteImage i
        _                -> Left "Can't convert image format to RGBA8"
                            -- TODO: Include source format in error message

toImageRes :: JP.Image JP.PixelRGBA8 -> Either String ImageRes
toImageRes jp =
    let w = JPT.imageWidth  jp
        h = JPT.imageHeight jp
        pixToWord32 (JP.PixelRGBA8 r g b a) =
            ((fromIntegral r) `shiftL` 0 ) .|.
            ((fromIntegral g) `shiftL` 8 ) .|.
            ((fromIntegral b) `shiftL` 16) .|.
            ((fromIntegral a) `shiftL` 24)
            :: Word32 
        convert = VS.create $ do
            v <- VSM.new $ w * h
            forM_ [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] $
                  \(x, y) -> VSM.write v
                                 (x + (h - 1 - y) * w) -- Flip image
                                 (pixToWord32 $ JP.pixelAt jp x y)
            return v
    in  if   w > 512 || h > 512 -- TODO: Hardcoded / arbitrary
        then Left "Image to large, won't convert"
        else Right $ ImageRes w h convert

-- Pop an uncached request of the request stack
popRequestStack :: ImageCache -> IO B.ByteString
popRequestStack ic = do
    let pop = atomically $ do
        requests <- readTVar $ icOutstandingReq ic
        let (requests', maybeURI) = LBM.deleteFindNewest requests
        case maybeURI of
            Just (uri, ()) ->
                do -- Write the stack with the removed top item back
                   writeTVar (icOutstandingReq ic) requests'
                   -- Make sure the request is not already in the cache / fetching
                   --
                   -- TODO: This should not be necessary, no duplicate requests in LRUBoundedMap,
                   --       hence there should never be anything in the request queue that's in
                   --       the cache
                   cache <- readTVar $ icCacheEntries ic
                   if   LBM.notMember uri cache
                   then do -- New request, mark fetch status and return URI
                           writeTVar (icCacheEntries ic) .
                               fst $ LBM.insertUnsafe uri Fetching cache
                           return $ Just uri
                   else return Nothing -- Already in cache, commit transaction here and return
                                       -- Nothing so the outer loop can try again
            Nothing  -> retry -- Empty request list, block till it makes sense to retry
    r <- pop
    case r of
        Nothing  -> popRequestStack ic -- Recurse till we get a URI or block on retry
        Just uri -> return uri

-- File or HTTP?
uriIsHTTP :: B.ByteString -> Bool
uriIsHTTP uri = B.isPrefixOf "http://" uri || B.isPrefixOf "https://" uri

-- Fetch an image into the disk cache (if not already there) and return it as a lazy ByteString
fetchDiskCache :: ImageCache -> Manager -> B.ByteString -> FilePath -> IO BL.ByteString 
fetchDiskCache ic manager uri cacheFn = do
    bs <- tryIOError $ BL.readFile cacheFn -- Already in the disk cache?
    case bs of
        Left ex -> do
            unless (isDoesNotExistError ex) $ -- Only handle the missing file ones silently
                throwIO ex
            -- No, fetch image
            if   uriIsHTTP uri
            then runResourceT $ do
                     req <- parseUrl $ B8.unpack uri
                     res <- httpLbs req manager
                     -- Store in disk cache
                     liftIO $ do
                         incCacheMisses ic
                         -- TODO: Misses HTTP protocol overhead
                         incCacheBytesTransf ic . fromIntegral . BL.length . responseBody $ res
                         -- BL.writeFile cacheFn $ responseBody res
                     return $ responseBody res
            else BL.readFile $ B8.unpack uri
        Right x -> incCacheDiskHits ic >> return x

modifyCacheEntry :: ImageCache
                 -> (LBM.Map B.ByteString CacheEntry -> LBM.Map B.ByteString CacheEntry)
                 -> IO ()
modifyCacheEntry ic f = atomically . modifyTVar' (icCacheEntries ic) $! f

data DecodeException = DecodeException { deError   :: String
                                       , deURI     :: String
                                       , deCacheFn :: String
                                       } deriving (Show, Typeable)
instance Exception DecodeException

fetchThread :: ImageCache -> Manager -> (forall a. IO a -> IO a) -> IO ()
fetchThread ic manager unmask =
  handle (\ThreadKilled -> -- Handle this exception here so we exit cleanly
             -- traceT TLInfo "Fetch thread received 'ThreadKilled', exiting"
             return ()
         ) . forever . unmask $ do
    -- The inner bracket takes care of cleanup, here we decide if the exception is
    -- recoverable or if we should stop the thread
    catches
      ( do
        -- Once we pop a request from the stack we own it, either fill it with valid image data
        -- or we'll mark it as a cache error
        bracketOnError
            (popRequestStack ic)
            (\uriUncached -> modifyCacheEntry ic $ LBM.update uriUncached CacheError)
            (\uriUncached -> do
                let cacheFn = B8.unpack $ mkURICacheFn ic uriUncached
                imgBS <- fetchDiskCache ic manager uriUncached cacheFn
                -- Decompress and convert
                --                                                         -- TODO: toStrict, nasty
                di <- case toImageRes =<< dynImgToRGBA8 =<< (JP.decodeImage $ BL.toStrict imgBS) of
                          Left  err -> throwIO $ DecodeException
                                           { deError   = err
                                           , deURI     = (B8.unpack uriUncached)
                                           , deCacheFn = cacheFn
                                           }
                          Right x   -> return x
                -- Update cache with image, make sure we actually decompress /
                -- covert it here instead of just storing a thunk
                di `seq` modifyCacheEntry ic $! LBM.update uriUncached (Fetched di)
            )
      )
      [ Handler $ \(ex :: IOException    ) -> reportEx ex
      , Handler $ \(ex :: HttpException  ) -> reportEx ex
      , Handler $ \(ex :: DecodeException) -> reportEx ex
      ]
    where reportEx ex = traceS TLError $ "Image Cache Exception: " ++ show ex
 
-- Return the image at the given URI from the cache, or schedule fetching if not present
fetchImage :: ImageCache -> B.ByteString -> IO (Maybe (CacheEntry))
fetchImage ic uri = do
    r <- atomically $ do
        cache <- readTVar (icCacheEntries ic)
        case LBM.lookup uri cache of
            (_,      Nothing) -> do -- New image, add it on top of the fetch stack
                                    modifyTVar' (icOutstandingReq ic) (fst . LBM.insert uri ())
                                    return Nothing
            (cache', e      ) -> do -- Write back cache with update LRU tick, return entry
                                    writeTVar (icCacheEntries ic) cache'
                                    return e
    case r of
        Just _  -> incCacheMemHits ic
        Nothing -> return ()
    return r

deleteImage :: ImageCache -> B.ByteString -> IO ()
deleteImage ic uri = modifyCacheEntry ic $ LBM.delete uri

-- Cache statistics

incCacheBytesTransf :: ImageCache -> Word64 -> IO ()
incCacheBytesTransf ic n = atomicModifyIORef' (icBytesTrans ic) (\b -> (b + n, ()))
incCacheMisses :: ImageCache -> IO ()
incCacheMisses ic = atomicModifyIORef' (icMisses ic) (\n -> (n + 1, ()))
incCacheDiskHits :: ImageCache -> IO ()
incCacheDiskHits ic = atomicModifyIORef' (icDiskHits ic) (\n -> (n + 1, ()))
incCacheMemHits :: ImageCache -> IO ()
incCacheMemHits ic = atomicModifyIORef' (icMemHits ic) (\n -> (n + 1, ()))

gatherCacheStats :: ImageCache -> IO String
gatherCacheStats ic = do
    bytesTransf <- readIORef $ icBytesTrans ic
    misses      <- readIORef $ icMisses     ic
    diskHits    <- readIORef $ icDiskHits   ic
    memHits     <- readIORef $ icMemHits    ic
    entries     <- atomically . readTVar $ icCacheEntries ic
    requests    <- atomically . readTVar $ icOutstandingReq ic
    let (fetching, fetched, cacheErr, mem) = foldr
         (\(_, (_, v)) (fetching', fetched', cacheErr', mem') -> case v of
             Fetching                 -> (fetching' + 1, fetched', cacheErr', mem')
             Fetched (ImageRes w h _) -> (fetching', fetched' + 1, cacheErr', mem' + w * h * 4)
             CacheError               -> (fetching', fetched', cacheErr' + 1, mem')
         )
         ((0, 0, 0, 0) :: (Word64, Word64, Word64, Int))
         (M.toList . fst . LBM.view $ entries)
    return $ printf
        (  "Image Cache - "
        ++ "Netw. Recv. Total: %.3fMB · Mem %.3fMB | "
        ++ "Req: %i/%i · Dir: %i/%i | "
        ++ "Misses: %i · DiskHits: %i · MemHits: %i | "
        ++ "Fetching: %i · Fetched: %i · Error: %i"
        )
        (fromIntegral bytesTransf / 1024 / 1024 :: Double)
        (fromIntegral mem         / 1024 / 1024 :: Double)
        (fst $ LBM.size requests)
        (snd $ LBM.size requests)
        (fst $ LBM.size entries)
        (snd $ LBM.size entries)
        misses
        diskHits
        memHits
        fetching
        fetched
        cacheErr

