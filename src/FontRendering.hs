
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
-- import Control.Applicative
import qualified Data.Vector.Storable as VS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IORef
import Data.Bits
import Data.Maybe

import qualified FT2Interface as FT2
import GLHelpers

-- OpenGL font rendering based on the FreeType 2 wrapper in FT2Interface

data FontRenderer = FontRenderer
    { frFT2              :: !FT2.FT2Library
    , frGlyphCache       :: !(IORef (HM.HashMap GlyphCacheKey GlyphCacheEntry))
    , frDefForceAutohint :: !Bool
    , frDefDisableKern   :: !Bool
    }

withFontRenderer :: Bool -> Bool -> (FontRenderer -> IO a) -> IO a
withFontRenderer frDefForceAutohint frDefDisableKern f =
    FT2.withFT2 $ \frFT2 -> -- We initialize our own FT2 library
        bracket ( do frGlyphCache <- newIORef HM.empty
                     return $ FontRenderer { .. }
                )
                ( -- Delete all OpenGL textures in the glyph cache
                  \fr -> readIORef (frGlyphCache fr)
                             >>= GL.deleteObjectNames
                                     . map (\(GlyphCacheEntry _ _ tex) -> tex) . HM.elems
                )
                f

-- Glyph Cache - We lookup glyphs based on a character code plus a typeface, and we store
--               glyph metrics and texture information

data GlyphCacheEntry = GlyphCacheEntry !FT2.GlyphMetrics !GL.TextureObject

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
getLoadedTypeface :: FontRenderer -> String -> Int -> IO (Maybe FT2.Typeface)
getLoadedTypeface fr = ft2ToFR fr FT2.getLoadedTypeface
loadTypeface      :: FontRenderer -> String -> Int -> Maybe Bool -> Maybe Bool -> IO FT2.Typeface
loadTypeface      fr fontFile pixelHeight mbForceAutohint mbDisableKern =
    (ft2ToFR fr FT2.loadTypeface)
        fontFile
        pixelHeight
        -- We have Maybe values for the autohint / kern options, falling back to our
        -- global defaults when they are not specified
        (fromMaybe (frDefForceAutohint fr) mbForceAutohint)
        (fromMaybe (frDefDisableKern   fr) mbDisableKern  )

drawText :: FontRenderer -> Int -> Int -> FT2.Typeface -> String -> IO ()
drawText fr x y face string = do
    -- Turn string into a list of glyphs, insert new glyphs into the cache if required
    glyphCache <- readIORef $ frGlyphCache fr
    (glyphCache', glyphs) <- foldM
        (\(cache, outGlyphs) c ->
            let key = mkGlyphCacheKey face c
            in  case HM.lookup key cache of
                    Just entry -> return (cache, entry : outGlyphs)
                    Nothing    -> -- New glyph, render it and upload texture data
                                  FT2.renderGlyph face c >>= \case
                                    (metrics@(FT2.GlyphMetrics { .. }), bitmap) -> do
                                      -- Texture
                                      tex <- uploadTexture2D
                                          GL.Alpha GL.Alpha' gWidth gHeight bitmap True
                                      -- Update cache
                                      let entry  = GlyphCacheEntry metrics tex
                                          cache' = HM.insert key entry cache
                                       in return (cache', entry : outGlyphs)
        ) (glyphCache, []) string
    writeIORef (frGlyphCache fr) glyphCache'
    -- Render glyphs
    forM_ glyphs $ \(GlyphCacheEntry (FT2.GlyphMetrics { .. }) tex) -> do
        return ()

-- Very basic and slow text rendering. Have FT2 render all the glyphs and draw them
-- directly using glDrawPixels
drawTextBitmap :: Int -> Int -> FT2.Typeface -> String -> IO ()
drawTextBitmap x y face string = do
    -- Our pixels are 8 bit, might not conform to the default 32 bit alignment
    GL.rowAlignment GL.Unpack GL.$= 1
    -- Draw black text
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- GL.depthMask GL.$= GL.Disabled
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
                      VS.unsafeWith bitmap (\ptr ->
                          GL.drawPixels (GL.Size (fromIntegral gWidth) (fromIntegral gHeight))
                                        (GL.PixelData GL.Alpha GL.UnsignedByte ptr))
                      return $ (xoffs + gAdvanceHorz + kernHorz, c)
        ) (fromIntegral x, toEnum 0) string
    -- GL.depthMask GL.$= GL.Enabled
    GL.blend GL.$= GL.Disabled

