
{-# LANGUAGE RecordWildCards #-}

module FontRendering ( withFontRenderer
                     , FontRenderer
                     , drawTextBitmap
                     , drawText
                     , gatherCacheStats
                       -- Wrap TextureAtlas exports
                     , debugDumpAtlas
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
import qualified Data.Vector.Storable as VS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IORef
import Data.Bits
import Data.Word
import Data.Maybe
import Text.Printf

import qualified FT2Interface as FT2
import qualified TextureAtlas as TA
import GLHelpers
import QuadRendering

-- OpenGL font rendering based on the FreeType 2 wrapper in FT2Interface

-- TODO: Add support for a fallback typeface which we use when another font lacks a glyph

data FontRenderer = FontRenderer
    { frFT2              :: !FT2.FT2Library
    , frTexAtlas         :: !TA.TextureAtlas
    , frGlyphCache       :: !(IORef (HM.HashMap GlyphCacheKey GlyphCacheEntry))
    , frKernCache        :: !(IORef (HM.HashMap KernCacheKey  Float          ))
    , frDefForceAutohint :: !Bool
    , frDefDisableKern   :: !Bool
    , frUseTexAtlas      :: !Bool
    }

withFontRenderer :: Bool -> Bool -> Bool -> (FontRenderer -> IO a) -> IO a
withFontRenderer frDefForceAutohint frDefDisableKern frUseTexAtlas f =
    FT2.withFT2 $ \frFT2 -> -- We initialize our own FT2 library
        TA.withTextureAtlas 512
                              1
                              GL.Alpha
                              GL.Alpha8
                              GL.UnsignedByte
                              (0 :: Word8)
                              TFMinMag
                              $ \frTexAtlas ->
            bracket
                ( do frGlyphCache <- newIORef HM.empty
                     frKernCache  <- newIORef HM.empty
                     return FontRenderer { .. }
                )
                ( \fr -> if   frUseTexAtlas
                         then -- Textures are managed by the texture atlas, don't delete them
                              return ()
                         else -- Delete all OpenGL textures in the glyph cache
                              readIORef (frGlyphCache fr)
                                  >>= GL.deleteObjectNames
                                          . map (\(GlyphCacheEntry _ _ tex _) -> tex) . HM.elems
                )
                f

-- Glyph Cache - We lookup glyphs based on a character code plus a typeface, and we store
--               glyph metrics and texture information

data GlyphCacheEntry = GlyphCacheEntry !Char !FT2.GlyphMetrics !GL.TextureObject !QuadUV

type GlyphCacheKey = Int

mkGlyphCacheKey :: FT2.Typeface -> Char -> GlyphCacheKey
mkGlyphCacheKey face c = let ha = hash face
                             hb = hash c
                         in  -- TODO: This is hopefully good enough. The Hashable instance for
                             --       Char is just 'fromEnum', and the one for Typeface just takes
                             --       the internal FT2 face pointer. With ASCII characters and
                             --       faces allocated in succession we might have all relevant
                             --       information in a few lower bits of the hash
                             (ha * hb) `xor` (ha * 997) `xor` (hb * 991)

-- Kerning Cache - Avoid several FT2 C API calls just to query kerning pair information.
--                 Should storage ever become an issue, we can replace this with a Bloom
--                 Filter as the vast majority of the time the kerning offset is zero

type KernCacheKey = Int

mkKernCacheKey :: FT2.Typeface -> Char -> Char -> KernCacheKey
mkKernCacheKey face left right = let ha = hash face
                                     hb = hash left
                                     hc = hash right
                                 in  (ha * hb * hc) `xor`
                                     (ha * 983)     `xor`
                                     (hb * 991)     `xor`
                                     (hc * 997)

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

debugDumpAtlas :: FontRenderer -> FilePath -> IO ()
debugDumpAtlas fr = TA.debugDumpAtlas (frTexAtlas fr)

stringToGlyphs :: FontRenderer -> FT2.Typeface -> String -> IO [GlyphCacheEntry]
stringToGlyphs (FontRenderer { .. }) face string = do
    -- Turn string into a list of glyphs, insert new glyphs into the cache if required
    -- TODO: Use LRUBoundedMap to retire elements when we reach some memory consumption cap
    (glyphCache, glyphs) <- readIORef frGlyphCache >>= \glyphCache -> foldM
        (\(cache, outGlyphs) c ->
            let key = mkGlyphCacheKey face c
            in  case HM.lookup key cache of
                    Just entry -> return (cache, entry : outGlyphs)
                    Nothing    ->
                        -- New glyph, render it and upload texture data
                        FT2.renderGlyph face c >>=
                          \(metrics@(FT2.GlyphMetrics { .. }), bitmap) -> do
                            entry <- case () of
                                _ | gWidth * gHeight == 0 -> -- Zero-area glyph (i.e. space)?
                                        return $ GlyphCacheEntry
                                            c metrics (GL.TextureObject 0) QuadUVDefault
                                  | frUseTexAtlas -> do -- Insert into texture atlas
                                        (tex, uv) <-
                                            TA.insertImage frTexAtlas
                                                           gWidth
                                                           gHeight
                                                           bitmap
                                        return $ GlyphCacheEntry
                                            c metrics tex uv
                                  | otherwise -> do -- Make new texture
                                        tex <- newTexture2D GL.Alpha
                                                            GL.Alpha8
                                                            GL.UnsignedByte
                                                            (gWidth, gHeight)
                                                            (TCUpload bitmap)
                                                            True
                                                            (Just TFMinMag)
                                                            True
                                        return $ GlyphCacheEntry
                                            c metrics tex QuadUVDefault
                            -- Update cache
                            return (HM.insert key entry cache, entry : outGlyphs)
        ) (glyphCache, []) string
    writeIORef frGlyphCache glyphCache
    return glyphs

-- Cached wrapper around FT2.getKerning
--
-- TODO: Combine this with stringToGlyphs so we have text layout cleanly
--       separated from drawing
getKerning :: FontRenderer -> FT2.Typeface -> Char -> Char -> IO Float
getKerning fr face left right =
    readIORef (frKernCache fr) >>= \kernCache ->
        let key = mkKernCacheKey face left right
        in  case HM.lookup key kernCache of
                Just n  -> return n
                Nothing -> do n <- FT2.getKerning face left right
                              writeIORef (frKernCache fr) $ HM.insert key n kernCache
                              return n

drawText :: FontRenderer -> QuadRenderBuffer -> Int -> Int -> FT2.Typeface -> String -> IO ()
drawText fr qb x y face string =
    -- Render glyphs
    stringToGlyphs fr face string >>= foldM_
        ( \(xoffs, prevc) (GlyphCacheEntry c (FT2.GlyphMetrics { .. }) tex uv) -> do
              -- Compute lower-left origin for glyph, taking into account kerning, bearing etc.
              kernHorz <- getKerning fr face prevc c
              when (gWidth * gHeight /= 0) $ -- Skip drawing empty glyphs
                  let x1 = round $ xoffs + fromIntegral gBearingX + kernHorz :: Int
                      y1 = y + (gBearingY - gHeight)
                      x2 = x1 + gWidth
                      y2 = y1 + gHeight
                   in -- Draw
                      drawQuad qb
                           (fromIntegral x1)
                           (fromIntegral y1)
                           (fromIntegral x2)
                           (fromIntegral y2)
                           1
                           FCBlack
                           TRSrcAlpha
                           (Just tex)
                           uv
              return (xoffs + gAdvanceHorz + kernHorz, c)
        ) (fromIntegral x, toEnum 0) . reverse

-- Very basic and slow text rendering. Have FT2 render all the glyphs and draw them
-- directly using glDrawPixels. Does not use the glyph cache
drawTextBitmap :: Int -> Int -> FT2.Typeface -> String -> IO ()
drawTextBitmap x y face string = do
    -- Our pixels are 8 bit, might not conform to the default 32 bit alignment
    GL.rowAlignment GL.Unpack GL.$= 1
    -- Draw black text
    GL.blend     GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- GL.depthMask GL.$= GL.Disabled
    foldM_
        ( \(xoffs, prevc) c ->
              FT2.renderGlyph face c >>=
                  \(FT2.GlyphMetrics { .. }, bitmap) -> do
                      -- Set lower-left origin for glyph, taking into account kerning, bearing etc.
                      kernHorz <- FT2.getKerning face prevc c
                      -- Debug print kerning pairs
                      -- when (kernHorz /= 0) . putStrLn $ [prevc, c, ' '] ++ show kernHorz
                      GL.windowPos (GL.Vertex2 (round $ xoffs + fromIntegral gBearingX + kernHorz)
                                               (fromIntegral $ y + (gBearingY - gHeight))
                                               :: GL.Vertex2 GL.GLint)
                      VS.unsafeWith bitmap
                          $ GL.drawPixels (GL.Size (fromIntegral gWidth) (fromIntegral gHeight))
                              . GL.PixelData GL.Alpha GL.UnsignedByte
                      return (xoffs + gAdvanceHorz + kernHorz, c)
        ) (fromIntegral x, toEnum 0) string
    -- GL.depthMask GL.$= GL.Enabled
    GL.blend GL.$= GL.Disabled

gatherCacheStats :: FontRenderer -> IO String
gatherCacheStats (FontRenderer { .. }) = do
    glyphCache                <- readIORef frGlyphCache
    kernCache                 <- readIORef frKernCache
    (numTex, wdhTex, ifmtTex) <- TA.getAtlasMemoryUsage frTexAtlas
    return $ printf
        "CachedGlyphs: %i · KernPairs: %i | AtlasTex: %i x %ix%ix%s"
        (HM.size glyphCache)
        (HM.size kernCache )
        numTex
        wdhTex wdhTex
        (show ifmtTex)

