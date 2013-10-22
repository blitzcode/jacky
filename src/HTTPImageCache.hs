
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module HTTPImageCache ( HTTPImageCache
                      , HTTPImageRes(..)
                      , CacheEntry(..)
                      , withHTTPImageCache
                      , fetchImage
                      ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import Data.Word
import Data.Bits
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
import Text.Printf

import BoundedStack
import Trace

-- Caching system (disk & memory) for image fetches over HTTP

data HTTPImageCache p =
    HTTPImageCache { hicCacheFolder    :: B.ByteString
                   , hicOutstandingReq :: TVar (BoundedStack B.ByteString)
                   , hicCacheEntries   :: TVar (M.Map B.ByteString (CacheEntry p))
                   }
-- TODO: Add cache statistics (disk hit rate, in-memory hit rate, bytes transferred, etc.)

data CacheEntry p = Fetching -- We keep in-progress entries in the cache to avoid double fetches
                  | Fetched HTTPImageRes
                  | Processed p -- User specified type to be stored in the cache. This allows our
                                -- HTTPImageRes to be processed into any custom representation
                                -- (OpenGL texture, summed area table etc.)
                  | CacheError  -- Failed to load / fetch / decode image

-- TODO: Add 'retryAfterNSec' field to CacheError to deal with failed fetches / decompression
--       while allowing to try again at some point

data HTTPImageRes = HTTPImageRes Int Int (VS.Vector Word32)

mkURLCacheFn :: HTTPImageCache p -> B.ByteString -> B.ByteString
mkURLCacheFn hic url = hicCacheFolder hic
                       <> (B8.pack . escapeURIString isUnescapedInURIComponent $ B8.unpack url)

withHTTPImageCache :: Manager
                   -> Int
                   -> String
                   -> (HTTPImageCache p -> IO ())
                   -> IO ()
withHTTPImageCache manager numConcReq cacheFolder f = do
    -- Make sure our cache folder exists
    createDirectoryIfMissing True cacheFolder
    -- Build record 
    initOutstandingReq <- newTVarIO $ mkBoundedStack 350 -- Limit outst. requests (TODO: hardcoded)
    initCacheEntries   <- newTVarIO $ M.empty
    let hic = HTTPImageCache { hicCacheFolder    = B8.pack $ addTrailingPathSeparator cacheFolder
                             , hicOutstandingReq = initOutstandingReq
                             , hicCacheEntries   = initCacheEntries
                             }
    bracket
        (forM [1..numConcReq] $ \_ -> async $ fetchThread hic manager) -- Launch fetch threads
        (\threads -> do
            traceT TLInfo "Shutting down image fetch threads"
            forM_ threads cancel
            forM_ threads $ \thread -> do
                r <- waitCatch thread
                case r of
                    Left ex -> traceS TLError $ printf "Exception from fetch thread '%s': %s"
                                                       (show $ asyncThreadId thread)
                                                       (show ex)
                    _        -> return ()
        )
        (\_ -> f hic)

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

toHTTPImageRes :: JP.Image JP.PixelRGBA8 -> HTTPImageRes
toHTTPImageRes jp =
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
    in  HTTPImageRes w h convert

-- Pop an uncached request of the request stack
popRequestStack :: HTTPImageCache p -> IO B.ByteString
popRequestStack hic = do
    let pop = atomically $ do
        requests <- readTVar $ hicOutstandingReq hic
        let (maybeURL, requests') = popBoundedStack requests
        case maybeURL of
            Just url ->
                do -- Write the stack with the removed top item back
                   writeTVar (hicOutstandingReq hic) requests'
                   -- Make sure the request is not already in the cache / fetching
                   cache <- readTVar $ hicCacheEntries hic
                   if   M.notMember url cache
                   then do -- New request, mark fetch status and return URL
                           writeTVar (hicCacheEntries hic) $ M.insert url Fetching cache
                           return $ Just url
                   else return Nothing -- Already in cache, commit transaction here and return
                                       -- Nothing so the outer loop can try again
            Nothing  -> retry -- Empty request list, block till it makes sense to retry
    r <- pop
    case r of
        Nothing  -> popRequestStack hic
        Just url -> return url

-- Fetch an image into the disk cache (if not already there) and return it as a lazy ByteString
fetchDiskCache :: Manager -> B.ByteString -> FilePath -> IO BL.ByteString 
fetchDiskCache manager url cacheFn = do
    bs <- tryIOError $ BL.readFile cacheFn -- Already in the disk cache?
    case bs of
        Left  _ -> -- No, fetch image from server
                   runResourceT $ do
                       req <- parseUrl $ B8.unpack url
                       res <- httpLbs req manager
                       -- Store in disk cache
                       liftIO . BL.writeFile cacheFn $ responseBody res
                       return $ responseBody res
        Right x -> return x

fetchThread :: HTTPImageCache p -> Manager -> IO ()
fetchThread hic manager = handle (\ThreadKilled -> -- Handle this exception here so we exit cleanly
                                     traceT TLInfo "Fetch thread received 'ThreadKilled'")
                          . forever $ do
    -- The inner bracket takes care of cleanup, here we decide if the exception is
    -- recoverable or if we should stop the thread
    catches
      ( do
        -- Once we pop a request from the stack we own it, either fill it with valid image data
        -- or we'll mark it as a cache error
        bracketOnError
            (popRequestStack hic)
            (\urlUncached -> updateCacheEntry hic urlUncached CacheError)
            (\urlUncached -> do
                let cacheFn = B8.unpack $ mkURLCacheFn hic urlUncached
                imgBS <- fetchDiskCache manager urlUncached cacheFn
                -- Decompress and convert
                --                                             TODO: Have to use toStrict, nasty
                di <- case dynImgToRGBA8 =<< (JP.decodeImage $ BL.toStrict imgBS) of
                          Left  err -> do
                              traceS TLError $
                                  printf "Error decoding image\nURL: %s\nCache File: %s\n%s"
                                         (B8.unpack urlUncached) cacheFn err
                              return $ JP.generateImage (\x y ->
                                  JP.PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) 64 64
                          Right x   -> return x
                -- Update cache with image
                -- TODO: Add strategy to retire old images from the cache
                updateCacheEntry hic urlUncached (Fetched $! toHTTPImageRes di)
            )
      )
      [ Handler (\(ex :: IOException  ) -> traceS TLError $ "HTTP Image Cache: " ++ show ex)
      , Handler (\(ex :: HttpException) -> traceS TLError $ "HTTP Image Cache: " ++ show ex)
      ]
    where
        updateCacheEntry :: HTTPImageCache p -> B.ByteString -> CacheEntry p -> IO ()
        updateCacheEntry hic' url entry = -- Can't re-use hic from the outer scope (type error)
            atomically . modifyTVar' (hicCacheEntries hic') $ \cache ->
                M.adjust (\_ -> entry)
                         url
                         cache
 
-- Return the image at the given URL from the cache, or schedule fetching if not present
-- TODO: Change structure of the request queue to avoid adding things twice
fetchImage :: HTTPImageCache p -> B.ByteString -> IO (Maybe (CacheEntry p))
fetchImage hic url = atomically $ do
    cache <- readTVar $ hicCacheEntries hic
    case M.lookup url cache of
        Nothing -> do -- New image, add it on top of the fetch stack
                      modifyTVar' (hicOutstandingReq hic) (pushBoundedStack url)
                      return Nothing
        e       -> return e

