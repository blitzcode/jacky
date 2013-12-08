
{-# LANGUAGE DeriveDataTypeable #-}

module FT2Interface ( withFT2
                    , FT2State
                    , getFT2Version
                    , loadTypeFace
                    , debugPrintTest
                    , FT2Exception(..)
                    ) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import Trace

-- Haskell interface for the FreeType 2 library. We use the ft2_interface.{c,h} for some
-- things that were more convenient to implement in C

data FT2Exception = FT2Exception { ft2Context :: String
                                 , ft2RetCode :: Int
                                 , ft2Message :: String
                                 } deriving (Show, Typeable)

instance Exception FT2Exception

-- Opaque pointers to FreeType structs
data FT2Face
type FT2FaceHandle = Ptr FT2Face
data FT2Library
type FT2LibraryHandle = Ptr FT2Library

data TypeFace = TypeFace { tfHandle :: FT2FaceHandle
                         }

data FT2State = FT2State { fsLibrary   :: FT2LibraryHandle
                         , fsTypeFaces :: IORef (HM.HashMap String TypeFace)
                         }

withFT2 :: (FT2State -> IO a) -> IO a
withFT2 f = do
    bracket
        ( -- FT_Init_FreeType takes a pointer to a FT_Library to initialize, which itself is
          -- just pointer to the internal library data structure. We just need to allocate a
          -- pointer and have the init function fill it in
          alloca $ \ptr -> do
              checkReturn "init" =<< c_FT_Init_FreeType ptr
              FT2State <$> (peek ptr) <*> newIORef HM.empty
        )
        ( \ft2 -> do traceS TLInfo "Shutting down FreeType"
                     faces <- readIORef $ fsTypeFaces ft2
                     forM_ (HM.elems faces) $ \face ->
                         checkReturn "shutdown" =<<
                             c_FT_Done_Face (tfHandle face)
                     checkReturn "shutdown" =<< c_FT_Done_FreeType (fsLibrary ft2)
        )
        f

loadTypeFace :: FT2State -> String -> String -> Int -> IO ()
loadTypeFace ft2 faceName fontFile pixelHeight = do
    faces <- readIORef $ fsTypeFaces ft2
    unless (HM.member faceName faces) $ -- Do nothing if there's a name collision
        withCString fontFile $ \fontFileCStr ->
            alloca $ \facePtr -> do
                cr =<< c_FT_New_Face (fsLibrary ft2) fontFileCStr 0 facePtr
                face <- peek facePtr
                onException -- Free the face if we fail here
                    ( cr =<< c_FT_Set_Pixel_Sizes face (fromIntegral pixelHeight) 0 )
                    ( cr =<< c_FT_Done_Face face )
                let newFace = TypeFace { tfHandle = face }
                writeIORef (fsTypeFaces ft2) $ HM.insert faceName newFace faces
    where cr = checkReturn ("loadTypeFace " ++ faceName)

-- Check a FT return value and throw an exception for errors
checkReturn :: String -> CInt -> IO ()
checkReturn ctx err | err == 0  = return ()
                    | otherwise = do errMsg <- peekCAString =<< c_errorToString err
                                     throwIO $ FT2Exception ctx (fromIntegral err) errMsg

getFT2Version :: FT2State -> IO (Int, Int, Int) -- Major, Minor, Patch
getFT2Version ft2 = do
    withArray [0, 0, 0] $ \ptr -> do
        c_FT_Library_Version
            (fsLibrary ft2)
            (ptr `advancePtr` 0)
            (ptr `advancePtr` 1)
            (ptr `advancePtr` 2)
        [major, minor, patch] <- peekArray 3 ptr
        return ( fromIntegral major
               , fromIntegral minor
               , fromIntegral patch
               )

debugPrintTest :: FT2State -> IO ()
debugPrintTest ft2 = checkReturn "debugPrintTest" =<< c_debugPrintTest (fsLibrary ft2)

-- FreeType 2 functions we call directly
foreign import ccall unsafe "FT_FREETYPE_H FT_Init_FreeType"
    c_FT_Init_FreeType :: Ptr FT2LibraryHandle -> IO CInt
foreign import ccall unsafe "FT_FREETYPE_H FT_Done_FreeType"
    c_FT_Done_FreeType :: FT2LibraryHandle -> IO CInt
foreign import ccall unsafe "FT_FREETYPE_H FT_Library_Version"
    c_FT_Library_Version :: FT2LibraryHandle -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "FT_FREETYPE_H FT_New_Face"
    c_FT_New_Face :: FT2LibraryHandle -> CString -> CLong -> Ptr FT2FaceHandle -> IO CInt
foreign import ccall unsafe "FT_FREETYPE_H FT_Set_Pixel_Sizes"
    c_FT_Set_Pixel_Sizes :: FT2FaceHandle -> CUInt -> CUInt -> IO CInt
foreign import ccall unsafe "FT_FREETYPE_H FT_Done_Face"
    c_FT_Done_Face :: FT2FaceHandle -> IO CInt

-- Functions from our C wrapper
foreign import ccall unsafe "ft2_interface.h errorToString"
    c_errorToString :: CInt -> IO CString
foreign import ccall unsafe "ft2_interface.h debugPrintTest"
    c_debugPrintTest :: FT2LibraryHandle -> IO CInt

