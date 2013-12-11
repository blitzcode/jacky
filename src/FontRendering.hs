
{-# LANGUAGE LambdaCase, RecordWildCards #-}

module FontRendering ( withFontRenderer
                     , FontRenderer
                     , drawTextBitmap
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
import Control.Monad.IO.Class
import Control.Exception
-- import Control.Applicative
import qualified Data.Vector.Storable as VS

import qualified FT2Interface as FT2

-- OpenGL font rendering based on the FreeType 2 wrapper in FT2Interface

data FontRenderer = FontRenderer { frFT2 :: FT2.FT2Library
                                 }

withFontRenderer :: (FontRenderer -> IO a) -> IO a
withFontRenderer f =
    FT2.withFT2 $ \ft2 -> -- We initialize our own FT2 library
        bracket ( return $ FontRenderer { frFT2 = ft2 } )
                ( \_ -> return () )
                f

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

-- Very basic and slow text rendering. -- Have FT2 render all the glyphs and draw them
-- using glDrawPixels
drawTextBitmap :: Int -> Int -> FT2.Typeface -> String -> IO ()
drawTextBitmap x y face string = do
    liftIO $ foldM_
        ( \(xoffs, prevc) c ->
              FT2.renderGlyph face c >>= \case
                  FT2.Glyph { .. } -> do
                      -- Set lower-left origin for glyph, taking into account kerning, bearing etc.
                      kernHorz <- FT2.getKerning face prevc c
                      -- Debug print kerning pairs
                      -- when (kernHorz /= 0) . putStrLn $ [c, prevc, ' '] ++ show kernHorz
                      GL.windowPos (GL.Vertex2 (fromIntegral $ xoffs + gBearingX + kernHorz)
                                               (fromIntegral $ y + (gBearingY - gHeight))
                                               :: GL.Vertex2 GL.GLint)
                      -- Our pixels are 8 bit, might not conform to the default 32 bit alignment
                      GL.rowAlignment GL.Unpack GL.$= 1
                      -- Draw black text
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                      -- GL.depthMask GL.$= GL.Disabled
                      VS.unsafeWith gBitmap (\ptr ->
                          GL.drawPixels (GL.Size (fromIntegral gWidth) (fromIntegral gHeight))
                                        (GL.PixelData GL.Alpha GL.UnsignedByte ptr)) 
                      -- GL.depthMask GL.$= GL.Enabled
                      GL.blend      GL.$= GL.Disabled
                      return $ (xoffs + gAdvanceHorz, c)
        ) (x, toEnum 0) string

