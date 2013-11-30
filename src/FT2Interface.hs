
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module FT2Interface ( withFT2
                    , FT2Exception(..)
                    ) where

import Data.Typeable
-- import Control.Applicative
import Control.Exception
import Foreign.C.Types
import Foreign.C.String

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
        ( checkReturn "init"     =<< initFreeType     )
        ( checkReturn "shutdown" =<< shutdownFreeType )
        f

-- Check a FT return value and throw an exception for errors
checkReturn :: String -> CInt -> IO ()
checkReturn ctx err | err == 0  = return ()
                    | otherwise = do errMsg <- peekCAString =<< errorToString err
                                     throwIO $ FT2Exception ctx (fromIntegral err) errMsg

foreign import ccall unsafe "ft2_interface.h initFreeType"
    initFreeType :: IO CInt
foreign import ccall unsafe "ft2_interface.h shutdownFreeType"
    shutdownFreeType :: IO CInt
foreign import ccall unsafe "ft2_interface.h errorToString"
    errorToString :: CInt -> IO CString

