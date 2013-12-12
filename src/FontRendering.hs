
{-# LANGUAGE LambdaCase, RecordWildCards #-}

module FontRendering ( withFontRenderer
                     , FontRenderer
                     , drawTextBitmap
                     , drawText
                       -- Pass through (or wrap) some FT2 exports
                     , debugPrintTest
                     , getFT2Version
                     , loadTypeface
                     , FT2.FT2Exception(..)
                     , getLoadedTypeface
                     , FT2.Typeface
                     ) where

import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Exception
import Control.Applicative
import qualified Data.Vector.Storable as VS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IORef
import Data.Bits

import qualified FT2Interface as FT2

-- OpenGL font rendering based on the FreeType 2 wrapper in FT2Interface

data FontRenderer = FontRenderer { frFT2        :: FT2.FT2Library
                                 , frGlyphCache :: IORef (HM.HashMap GlyphCacheKey GlyphCacheEntry)
                                 }

withFontRenderer :: (FontRenderer -> IO a) -> IO a
withFontRenderer f =
    FT2.withFT2 $ \ft2 -> -- We initialize our own FT2 library
        bracket ( FontRenderer <$> pure ft2 <*> newIORef HM.empty)
                ( \_ -> return () )
                f

-- Glyph Cache - We lookup glyphs based on a character code plus a typeface, and we store
--               glyph metrics and texture information

data GlyphCacheEntry = GlyphCacheEntry !FT2.GlyphMetrics !Int

type GlyphCacheKey = Int

mkGlyphCacheKey :: FT2.Typeface -> Char -> GlyphCacheKey
mkGlyphCacheKey face c = hash face `xor` hash c

-- For re-exporting functions from FT2 taking our FontRenderer instead of the internal FT2Library
ft2ToFR :: FontRenderer -> (FT2.FT2Library -> b) -> b
ft2ToFR fr f = f (frFT2 fr)

debugPrintTest    :: FontRenderer -> IO ()
debugPrintTest    fr = ft2ToFR fr FT2.debugPrintTest
getFT2Version     :: FontRenderer -> IO (Int, Int, Int)
getFT2Version     fr = ft2ToFR fr FT2.getFT2Version
loadTypeface      :: FontRenderer -> String -> Int -> IO FT2.Typeface
loadTypeface      fr = ft2ToFR fr FT2.loadTypeface
getLoadedTypeface :: FontRenderer -> String -> Int -> IO (Maybe FT2.Typeface)
getLoadedTypeface fr = ft2ToFR fr FT2.getLoadedTypeface

drawText :: FontRenderer -> Int -> Int -> FT2.Typeface -> String -> IO ()
drawText fr x y face string = do
    -- Turn string into a list of glyphs, insert new glyphs into the cache if required
    glyphCache <- readIORef $ frGlyphCache fr
    (glyphCache', glyphs) <- foldM
        (\(cache, outGlyphs) c ->
            let key = mkGlyphCacheKey face c
            in  case HM.lookup key cache of
                    Just entry -> return (glyphCache, entry : outGlyphs)
                    Nothing    -> FT2.renderGlyph face c >>= \case
                                      (gm@FT2.GlyphMetrics { .. }, bitmap) -> do
                                          return (glyphCache, outGlyphs)
        ) (glyphCache, []) string
    writeIORef (frGlyphCache fr) glyphCache'

-- Very basic and slow text rendering. Have FT2 render all the glyphs and draw them
-- directly using glDrawPixels
drawTextBitmap :: Int -> Int -> FT2.Typeface -> String -> IO ()
drawTextBitmap x y face string =
    foldM_
        ( \(xoffs, prevc) c ->
              FT2.renderGlyph face c >>= \case
                  (FT2.GlyphMetrics { .. }, bitmap) -> do
                      -- Set lower-left origin for glyph, taking into account kerning, bearing etc.
                      kernHorz <- FT2.getKerning face prevc c
                      -- Debug print kerning pairs
                      -- when (kernHorz /= 0) . putStrLn $ [prevc, c, ' '] ++ show kernHorz
                      GL.windowPos (GL.Vertex2 (round $ xoffs + (fromIntegral gBearingX) + kernHorz)
                                               (fromIntegral $ y + (gBearingY - gHeight))
                                               :: GL.Vertex2 GL.GLint)
                      -- Our pixels are 8 bit, might not conform to the default 32 bit alignment
                      GL.rowAlignment GL.Unpack GL.$= 1
                      -- Draw black text
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                      -- GL.depthMask GL.$= GL.Disabled
                      VS.unsafeWith bitmap (\ptr ->
                          GL.drawPixels (GL.Size (fromIntegral gWidth) (fromIntegral gHeight))
                                        (GL.PixelData GL.Alpha GL.UnsignedByte ptr))
                      -- GL.depthMask GL.$= GL.Enabled
                      GL.blend      GL.$= GL.Disabled
                      return $ (xoffs + gAdvanceHorz + kernHorz, c)
        ) (fromIntegral x, toEnum 0) string

