
{-# LANGUAGE   ScopedTypeVariables
             , OverloadedStrings
             , DeriveDataTypeable
             , RecordWildCards
             , RankNTypes #-}

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
import Timing

-- Caching system (disk & memory) for image fetches over HTTP and from disk

-- TODO: Add support for retiring elements from the disk cache, consider not having the disk
--       cache at all and only use it to speed up application startup / offline mode

-- TODO: Now that all image data ends up in the texture cache, this is mostly a
--       request queue and a staging area, with some record keeping for fetches
--       / errors. Maybe we can simplify and speed up some things?

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
                | CacheError -- Failed to load / fetch / decode image
                      !Double -- Tick after which we're allowed to retry
                      !Int    -- Number of retry attempt scheduled next
                deriving Eq

instance Show CacheEntry where
    show Fetching                            = "Fetching"
    show (Fetched _)                         = "Fetched"
    show (CacheError retryTick retryAttempt) =
        "CacheError " ++ show retryTick ++ " " ++ show retryAttempt

data ImageRes = ImageRes !Int !Int !(VS.Vector Word32)
                         deriving Eq

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
    ic <- do
        icOutstandingReq <- newTVarIO $ LBM.empty $ memCacheEntryLimit `div` 2
        icCacheEntries   <- newTVarIO $ LBM.empty memCacheEntryLimit
        [icBytesTrans, icMisses, icDiskHits, icMemHits]
            <- forM ([1..4] :: [Int]) $ \_ -> newIORef (0 :: Word64)
        return $ ImageCache { icCacheFolder = B8.pack $ addTrailingPathSeparator cacheFolder
                            , ..
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
            case req   of Just err -> traceS TLError $ "LRUBoundedMap: icOutstandingReq:\n" ++ err
                          Nothing  -> return ()
            case cache of Just err -> traceS TLError $ "LRUBoundedMap: icCacheEntries:\n"   ++ err
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
        then Left  $ printf "Image to large (%ix%i), won't convert" w h
        else Right $ ImageRes w h convert

-- Pop an uncached request of the request stack
popRequestStack :: ImageCache
                -> IO (B.ByteString, Int) -- Return URI and the retry attempt number we're on
popRequestStack ic = do
    let pop = atomically $ do
          requests <- readTVar $ icOutstandingReq ic
          let (requests', maybeURI) = LBM.popNewest requests
          case maybeURI of
              Just (uri, ()) ->
                  do -- Write the stack with the removed top item back
                     writeTVar (icOutstandingReq ic) requests'
                     -- Check if the request is already in the cache
                     cache <- readTVar $ icCacheEntries ic
                     case LBM.lookupNoLRU uri cache of
                         Nothing -> do
                             -- New request, mark fetch status and return URI
                             writeTVar (icCacheEntries ic) .
                                 fst $ LBM.insert uri Fetching cache
                             return $ Just (uri, 0) -- No retries
                         Just (CacheError _ retryAttempt) -> do
                             -- Marked as an error in the cache, update to fetching and
                             -- return current retry attempt
                             writeTVar (icCacheEntries ic) $ LBM.update uri Fetching cache
                             return $ Just (uri, retryAttempt)
                         Just entry -> -- TODO: Maybe just commit transaction and trace instead?
                                       error $ "popRequestStack internal error: '"
                                           ++ B8.unpack uri ++ "' in request queue, but already "
                                           ++ "marked as '" ++ (show entry) ++ "' in cache"
              Nothing        -> retry -- Empty request list, block till it makes sense to retry
    r <- pop
    case r of
        Nothing  -> popRequestStack ic -- Recurse till we get a URI or block on retry
        Just res -> return res

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
                         -- TODO: Disk cache writes disabled for now
                         -- BL.writeFile cacheFn $ responseBody res
                     return $ responseBody res
            else BL.readFile $ B8.unpack uri -- TODO: We should restrict fetches of disk files
                                             --       to certain directories, might be a security
                                             --       issue otherwise
        Right x -> incCacheDiskHits ic >> return x

modifyCacheEntries :: ImageCache
                   -> (LBM.Map B.ByteString CacheEntry -> LBM.Map B.ByteString CacheEntry)
                   -> IO ()
modifyCacheEntries ic f = atomically . modifyTVar' (icCacheEntries ic) $! f

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
        -- Once we pop a request from the stack we own it, either fill it with valid
        -- image data or mark it as a cache error. Note that we never want to call
        -- LBM.insert on the cache directory, only LBM.update. Our entry might have
        -- been removed while we're fetching it, don't add it back
        bracketOnError
            (popRequestStack ic)
            (\(uriUncached, retryAttempt) -> do
                time <- (+ (retryDelay retryAttempt)) <$> getTick
                modifyCacheEntries ic . LBM.update uriUncached $ CacheError time (retryAttempt + 1)
            )
            (\(uriUncached, retryAttempt) -> do
                when (retryAttempt > 0) .
                    traceS TLWarn $ printf
                        "Now attempting retry no. %i of failed URI fetch after >=%.1fsec delay: %s"
                        retryAttempt
                        (retryDelay $ retryAttempt - 1)
                        (B8.unpack uriUncached)
                let cacheFn = B8.unpack $ mkURICacheFn ic uriUncached
                imgBS <- fetchDiskCache ic manager uriUncached cacheFn
                -- Decompress and convert
                di <- {-# SCC decompressAndConvert #-}                     -- TODO: toStrict, nasty
                      case toImageRes =<< dynImgToRGBA8 =<< (JP.decodeImage $ BL.toStrict imgBS) of
                          Left  err -> throwIO $ DecodeException
                                           { deError   = err
                                           , deURI     = (B8.unpack uriUncached)
                                           , deCacheFn = cacheFn
                                           }
                          Right x   -> return x
                -- Update cache with image, make sure we actually decompress /
                -- covert it here instead of just storing a thunk
                di `seq` modifyCacheEntries ic $! LBM.update uriUncached (Fetched di)
            )
      )
      [ Handler $ \(ex :: IOException    ) -> reportEx ex
      , Handler $ \(ex :: HttpException  ) -> reportEx ex
      , Handler $ \(ex :: DecodeException) -> reportEx ex
      ]
    where reportEx ex             = traceS TLError $ "Image Cache Exception: " ++ show ex
          retryDelay retryAttempt = ([2, 10, 30, 60, 120] ++ repeat 600) !! retryAttempt :: Double

-- Return the image at the given URI from the cache, or schedule fetching if not present
fetchImage :: ImageCache -> Double -> B.ByteString -> IO (Maybe CacheEntry)
fetchImage ic tick uri = do
    r <- atomically $ do
        cache <- readTVar (icCacheEntries ic)
        case LBM.lookup uri cache of
            (_,      Nothing) -> addRequest >> return Nothing -- New image, add request
            (cache', e@(Just (CacheError retryTick _))) -> do
                when (tick > retryTick)
                    -- Time to retry this failed fetch, add to fetch queue. Note that we don't
                    -- remove the error from the cache, we want to keep the retry count around
                    addRequest
                writeTVar (icCacheEntries ic) cache' -- Update LRU
                return e
            (cache', e) -> -- Hit, just update LRU
                           writeTVar (icCacheEntries ic) cache' >> return e
    case r of
        Just _  -> incCacheMemHits ic
        Nothing -> return ()
    return r
  where addRequest = modifyTVar' (icOutstandingReq ic) (fst . LBM.insert uri ())

deleteImage :: ImageCache -> B.ByteString -> IO ()
deleteImage ic uri = modifyCacheEntries ic (fst . LBM.delete uri)

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
         (\(_, v) (fetching', fetched', cacheErr', mem') -> case v of
             Fetching                 -> (fetching' + 1, fetched', cacheErr', mem')
             Fetched (ImageRes w h _) -> (fetching', fetched' + 1, cacheErr', mem' + w * h * 4)
             CacheError _ _           -> (fetching', fetched', cacheErr' + 1, mem')
         )
         ((0, 0, 0, 0) :: (Word64, Word64, Word64, Int))
         (LBM.toList entries)
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

