
module FT2Interface ( initFreeType
                    ) where

-- import Foreign.C.Types
-- import Foreign.Ptr

-- Haskell wrapper around our ft2_interface.{h,cpp} C++ interface for the
-- FreeType 2 library

foreign import ccall unsafe "ft2_interface.h initFreeType" initFreeType :: IO ()

