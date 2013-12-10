
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module FT2Interface ( withFT2
                    , FT2Library
                    , getFT2Version
                    , loadTypeface
                    , Typeface
                    , renderGlyph
                    , Glyph(..)
                    , getKerning
                    , getLoadedTypeface
                    , debugPrintTest
                    , FT2Exception(..)
                    ) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Data.Word
import Data.Bits
import Data.List
import Text.Printf
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Exception
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.ForeignPtr.Safe

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
data FT2Library'
type FT2LibraryHandle = Ptr FT2Library'

data Typeface = Typeface { tfHandle     :: FT2FaceHandle
                         , tfFamilyName :: String
                         , tfReqHeight  :: Int
                         }

data FT2Library = FT2Library { flLibrary   :: FT2LibraryHandle
                             , flTypefaces :: IORef [Typeface] -- So we can free them on exit
                             }

withFT2 :: (FT2Library -> IO a) -> IO a
withFT2 f = do
    bracket
        ( -- FT_Init_FreeType takes a pointer to a FT_Library to initialize, which itself is
          -- just pointer to the internal library data structure. We just need to allocate a
          -- pointer and have the init function fill it in
          alloca $ \ptr -> do
              checkReturn "init" =<< c_FT_Init_FreeType ptr
              FT2Library <$> (peek ptr) <*> newIORef []
        )
        ( \ft2 -> do traceS TLInfo "Shutting down FreeType"
                     faces <- readIORef $ flTypefaces ft2
                     forM_ faces $ \face ->
                         checkReturn "shutdown" =<<
                             c_FT_Done_Face (tfHandle face)
                     checkReturn "shutdown" =<< c_FT_Done_FreeType (flLibrary ft2)
        )
        f

-- Load a typeface at a given size and return its opaque record
loadTypeface :: FT2Library -> String -> Int -> IO Typeface
loadTypeface ft2 fontFile pixelHeight = do
    faces <- readIORef $ flTypefaces ft2
    withCString fontFile $ \fontFileCStr ->
        alloca $ \facePtr -> do
            cr =<< c_FT_New_Face (flLibrary ft2) fontFileCStr 0 facePtr
            face <- peek facePtr
            onException -- Free the face if we fail here
                ( cr =<< c_FT_Set_Pixel_Sizes face (fromIntegral pixelHeight) 0 )
                ( cr =<< c_FT_Done_Face face )
            -- Trace what we just loaded
            with     nullPtr $ \familyName ->
                with nullPtr $ \styleName  ->
                with 0       $ \numGlyphs  ->
                with 0       $ \hasKerning ->
                with 0       $ \height     ->
                with 0       $ \ascender   ->
                with 0       $ \descender  -> do
                    c_faceInfo face
                               familyName
                               styleName
                               numGlyphs
                               hasKerning
                               height
                               ascender
                               descender
                    tfFamilyName <- peekCAString =<< peek familyName
                    traceS TLInfo =<< printf
                        "Font: %s, %s · Hgt/Asc/Dsc: %i/%i/%ipx · %iGlyphs · %s"
                        <$> pure tfFamilyName
                        <*> (peekCAString =<< peek styleName)
                        <*> (fromIntegral <$> peek height    :: IO Int)
                        <*> (fromIntegral <$> peek ascender  :: IO Int)
                        <*> (fromIntegral <$> peek descender :: IO Int)
                        <*> (fromIntegral <$> peek numGlyphs :: IO Int)
                        <*> ((\x -> if   x /= 0
                                    then "Kern"
                                    else "NoKern") <$> peek hasKerning)
                    -- Store in library record
                    let newFace = Typeface { tfHandle    = face
                                           , tfReqHeight = pixelHeight
                                           , ..
                                           }
                    writeIORef (flTypefaces ft2) $ newFace : faces
                    return newFace
    where cr = checkReturn ("loadTypeface " ++ fontFile)

-- Retrieve a typeface by its family name and size
getLoadedTypeface :: FT2Library -> String -> Int -> IO (Maybe Typeface)
getLoadedTypeface ft2 faceName pixelHeight = do
    faces <- readIORef $ flTypefaces ft2
    return $ find (\face -> tfFamilyName face == faceName &&
                            tfReqHeight  face == pixelHeight) faces

data Glyph = Glyph { -- Glyph metrics
                     -- http://www.freetype.org/freetype2/docs/tutorial/metrics.png
                     --
                     gAdvanceHorz :: Int             -- Horizontal offset for the next glyph
                   , gBearingX    :: Int             -- X & Y offset for positioning the bitmap
                   , gBearingY    :: Int             --   relative to the current pen position
                     -- Glyph bitmap
                   , gBitmap      :: VS.Vector Word8 -- Glyph grayscale image
                   , gWidth       :: Int             -- Dimensions of glyph image
                   , gHeight      :: Int             -- ...
                   }

renderGlyph :: Typeface -> Char -> IO Glyph
renderGlyph face c = do
    -- Call C wrapper
    with     0       $ \advanceHorz ->
        with 0       $ \bearingX    ->
        with 0       $ \bearingY    ->
        with 0       $ \bitmapWidth ->
        with 0       $ \bitmapPitch ->
        with 0       $ \bitmapRows  ->
        with nullPtr $ \bitmap      -> do
            checkReturn "renderGlyph" =<<
                c_renderGlyph (tfHandle face)
                              (fromIntegral $ fromEnum c)
                              advanceHorz
                              bearingX
                              bearingY
                              bitmapWidth
                              bitmapPitch
                              bitmapRows
                              bitmap
            -- Extract data passed back through pointers
            gAdvanceHorz <- fromIntegral <$> peek advanceHorz
            gBearingX    <- fromIntegral <$> peek bearingX
            gBearingY    <- fromIntegral <$> peek bearingY
            gWidth       <- fromIntegral <$> peek bitmapWidth
            pitch        <- fromIntegral <$> peek bitmapPitch
            gHeight      <- fromIntegral <$> peek bitmapRows
            -- Copy and convert (pitch == width) bitmap image
            bitmapVS     <- VS.unsafeFromForeignPtr0
                                <$> (newForeignPtr_ =<< peek bitmap)
                                <*> pure (pitch * gHeight)
            let gBitmap = VS.create $ do
                    v <- VSM.new $ gWidth * gHeight
                    forM_ [(x, y) | y <- [0..gHeight - 1], x <- [0..gWidth - 1]]
                        $ \(x, y) ->
                            VSM.write v (x + y * gWidth) . fromIntegral $
                                bitmapVS VS.! (x + (gHeight - 1 - y) * pitch) -- Flip
                    return v
            -- TODO: The bitmap returned by renderGlyph_c is from the face's
            --       glyph slot and will be overwritten on the next invocation.
            --       Do we need to add more strictness to ensure that gBitmap
            --       does no longer depend on the bitmapVS wrapper around that
            --       storage?
            gBitmap `seq` return $ Glyph { .. }

-- Return horizontal kerning for the two passed characters in the given typeface
--
-- TODO: It seems that FT2 can't actually use the type of kerning information present in
--       most fonts (kern vs GPOS table)
--
getKerning :: Typeface -> Char -> Char -> IO Int
getKerning face left right = do
    [leftIdx, rightIdx] <-
        mapM (c_FT_Get_Char_Index (tfHandle face) . fromIntegral . fromEnum) [left, right]
    withArray [0, 0] $ \kerningVec -> do
        checkReturn "getKerning" =<<
            c_FT_Get_Kerning (tfHandle face) leftIdx rightIdx 0 kerningVec
        kernHorz <- fromIntegral <$> peek kerningVec
        return $ kernHorz `shiftR` 6

-- Check a FT return value and throw an exception for errors
checkReturn :: String -> CInt -> IO ()
checkReturn ctx err | err == 0  = return ()
                    | otherwise = do errMsg <- peekCAString =<< c_errorToString err
                                     throwIO $ FT2Exception ctx (fromIntegral err) errMsg

getFT2Version :: FT2Library -> IO (Int, Int, Int) -- Major, Minor, Patch
getFT2Version ft2 = do
    withArray [0, 0, 0] $ \ptr -> do
        c_FT_Library_Version
            (flLibrary ft2)
            (ptr `advancePtr` 0)
            (ptr `advancePtr` 1)
            (ptr `advancePtr` 2)
        [major, minor, patch] <- peekArray 3 ptr
        return ( fromIntegral major
               , fromIntegral minor
               , fromIntegral patch
               )

debugPrintTest :: FT2Library -> IO ()
debugPrintTest ft2 = checkReturn "debugPrintTest" =<< c_debugPrintTest (flLibrary ft2)

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
foreign import ccall unsafe "FT_FREETYPE_H FT_Get_Char_Index"
    c_FT_Get_Char_Index :: FT2FaceHandle -> CULong -> IO CUInt
foreign import ccall unsafe "FT_FREETYPE_H FT_Get_Kerning"
    c_FT_Get_Kerning :: FT2FaceHandle -> CUInt -> CUInt -> CUInt -> Ptr CLong -> IO CInt

-- Functions from our C wrapper
foreign import ccall unsafe "ft2_interface.h errorToString"
    c_errorToString :: CInt -> IO CString
foreign import ccall unsafe "ft2_interface.h debugPrintTest"
    c_debugPrintTest :: FT2LibraryHandle -> IO CInt
foreign import ccall unsafe "ft2_interface.h renderGlyph"
    c_renderGlyph :: FT2FaceHandle
                  -> CULong
                  -> Ptr CUInt
                  -> Ptr CInt
                  -> Ptr CInt
                  -> Ptr CUInt
                  -> Ptr CUInt
                  -> Ptr CUInt
                  -> Ptr (Ptr CUChar)
                  -> IO CInt
foreign import ccall unsafe "ft2_interface.h faceInfo"
    c_faceInfo :: FT2FaceHandle
                  -> Ptr CString
                  -> Ptr CString
                  -> Ptr CLong
                  -> Ptr CInt
                  -> Ptr CShort
                  -> Ptr CShort
                  -> Ptr CShort
                  -> IO ()

