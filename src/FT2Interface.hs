
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module FT2Interface ( withFT2
                    , libraryVersion
                    , FT2Exception(..)
                    ) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Control.Exception
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- Haskell wrapper around our ft2_interface.{h,cpp} C++ interface for the
-- FreeType 2 library

data FT2Exception = FT2Exception { ft2Context :: String
                                 , ft2RetCode :: Int
                                 , ft2Message :: String
                                 } deriving (Show, Typeable)

instance Exception FT2Exception

withFT2 :: (IO ()) -> IO ()
withFT2 f = do
    bracket_
        ( checkReturn "init"     =<< c_initFreeType     )
        ( checkReturn "shutdown" =<< c_shutdownFreeType )
        f

-- Check a FT return value and throw an exception for errors
checkReturn :: String -> CInt -> IO ()
checkReturn ctx err | err == 0  = return ()
                    | otherwise = do errMsg <- peekCAString =<< c_errorToString err
                                     throwIO $ FT2Exception ctx (fromIntegral err) errMsg

libraryVersion :: IO (Int, Int, Int) -- Major, Minor, Patch
libraryVersion = do
    ver <- c_libraryVersion
    if   ver == nullPtr
    then return (0, 0, 0)
    else do [major, minor, patch] <- forM [0..2] $ \i -> fromIntegral <$> peek (ver `advancePtr` i)
            return (major, minor, patch)

foreign import ccall unsafe "ft2_interface.h initFreeType"
    c_initFreeType :: IO CInt
foreign import ccall unsafe "ft2_interface.h shutdownFreeType"
    c_shutdownFreeType :: IO CInt
foreign import ccall unsafe "ft2_interface.h errorToString"
    c_errorToString :: CInt -> IO CString
foreign import ccall unsafe "ft2_interface.h libraryVersion"
    c_libraryVersion :: IO (Ptr CInt)

