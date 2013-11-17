
module FT2Interface ( withFT2
                    ) where

import Control.Exception
-- import Foreign.C.Types
-- import Foreign.Ptr

-- Haskell wrapper around our ft2_interface.{h,cpp} C++ interface for the
-- FreeType 2 library

withFT2 :: (IO ()) -> IO ()
withFT2 f = do
    bracket_
        ( initFreeType     )
        ( shutdownFreeType )
        f

foreign import ccall unsafe "ft2_interface.h initFreeType" initFreeType :: IO ()
foreign import ccall unsafe "ft2_interface.h shutdownFreeType" shutdownFreeType :: IO ()

