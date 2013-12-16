
{-# LANGUAGE RecordWildCards #-}

module GLQuadRendering ( withGLQuadRenderer
                       , GLQuadRenderer
                       , RGBA(..)
                       , FillColor(..)
                       , FillTransparency(..)
                       , drawQuadImmediate
                       ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.List
import Control.Exception
import Control.Monad
-- import Control.Applicative
-- import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import GLImmediate

-- Module for efficient rendering of 2D quad primitives, used for UI elements and texture
-- mapped font rendering

data GLQuadRenderer = GLQuadRenderer { qrVtxBuf :: !GL.BufferObject
                                     , qrColBuf :: !GL.BufferObject
                                     , qrUVBuf  :: !GL.BufferObject
                                     }

withGLQuadRenderer :: (GLQuadRenderer -> IO a) -> IO a
withGLQuadRenderer =
    bracket ( do [qrVtxBuf, qrColBuf, qrUVBuf] <- GL.genObjectNames 3
                 return $ GLQuadRenderer { .. }
            )
            ( \GLQuadRenderer { .. } -> GL.deleteObjectNames [qrVtxBuf, qrColBuf, qrUVBuf] )

data RGBA = RGBA {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Show)

data FillColor = FCWhite
               | FCBlack
               | FCSolid !RGBA
               | FCBottomTopGradient !RGBA !RGBA
               | FCLeftRightGradient !RGBA !RGBA
               deriving (Show)

data FillTransparency = FTNone
                      | FTBlend !Float
                      | FTSrcAlpha
                      deriving (Show)

-- Simple / slow / obsolete immediate mode drawing of quads

drawQuadImmediate :: Float
         -> Float
         -> Float
         -> Float
         -> Float
         -> FillColor
         -> FillTransparency
         -> Maybe GL.TextureObject
         -> IO ()
drawQuadImmediate x1 y1 x2 y2 depth col trans tex = do
    let pos' = [ (x1, y1), (x2, y1), (x2, y2), (x1, y2) ]
        cols = case col of FCWhite                 -> replicate 4 (RGBA 1 1 1 1)
                           FCBlack                 -> replicate 4 (RGBA 0 0 0 1)
                           FCSolid c               -> replicate 4 c
                           FCBottomTopGradient b t -> [b, b, t, t]
                           FCLeftRightGradient l r -> [l, r, l, r]
        texs = [ (0, 0), (1, 0), (1, 1), (0, 1) ]
    case trans of FTNone         -> GL.blend GL.$= GL.Disabled
                  FTBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat)
                  FTSrcAlpha     -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    case tex of Just _ -> do
                    -- TODO: Measure speedup from avoiding texture changes, then consider
                    --       packing textures. Will probably need to do that for text
                    --       rendering anyway
                    GL.texture         GL.Texture2D      GL.$= GL.Enabled
                    GL.textureBinding  GL.Texture2D      GL.$= tex
                    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
                    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
                    -- TODO: Disable magnification filter if we're mapping pixels and texels
                    --       1:1. Some GPUs introduce blurriness otherwise
                    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                Nothing -> GL.texture GL.Texture2D GL.$= GL.Disabled

    {-
    GL.renderPrimitive GL.Quads . forM_ (zip3 pos' cols texs) $
        \((x, y), RGBA r g b a, (u, v)) -> do
            color4f r g b a
            texCoord2f u v
            vertex3f x y (-depth)
    -}


    {-
    -- Create and bind Vertex Array Object (VAO)
    [vao] <- GL.genObjectNames 1
    GL.bindVertexArrayObject GL.$= Just vao
    -}

    let szf       = sizeOf (0 :: Float)
        numvtx    = 4
        vtxStride = 3
        colStride = 4
        uvStride  = 2
        vtxSize   = numvtx * vtxStride
        colSize   = numvtx * colStride
        uvSize    = numvtx * uvStride
        totalSize = vtxSize + colSize + uvSize
        vtxOffset = 0
        colOffset = vtxOffset + vtxSize
        uvOffset  = colOffset + colSize

    -- Create, bind and allocate Vertex Buffer Object (VBO)
    [vbo] <- GL.genObjectNames 1
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
    GL.bufferData GL.ArrayBuffer GL.$= ( fromIntegral $ totalSize * szf
                                       , nullPtr
                                       , GL.StreamDraw
                                       )

    -- Fill mapped VBO with vertex data
    GL.withMappedBuffer
       GL.ArrayBuffer
       GL.WriteOnly
       ( \ptr -> newForeignPtr_ ptr >>= \fp -> do
            let vec = VSM.unsafeFromForeignPtr fp vtxOffset vtxSize in
                forM_ (zip [0..] pos') $ \(i, (x, y)) ->
                    forM_ (zip [0..] [x, y, (-depth)]) $ \(c, f) ->
                        VSM.write vec (i * vtxStride + c) f
            let vec = VSM.unsafeFromForeignPtr fp colOffset colSize in
                forM_ (zip [0..] cols) $ \(i, RGBA r g b a) ->
                    forM_ (zip [0..] [r, g, b, a]) $ \(c, f) ->
                        VSM.write vec (i * colStride + c) f
            let vec = VSM.unsafeFromForeignPtr fp uvOffset uvSize in
                forM_ (zip [0..] texs) $ \(i, (u, v)) ->
                    forM_ (zip [0..] [u, v]) $ \(c, f) ->
                        VSM.write vec (i * uvStride + c) f
       )
       ( \mf -> error $ "drawQuadImmediate - OpenGL mapping failure: " ++ show mf )

    -- Specify and enable vertex arrays
    GL.arrayPointer GL.VertexArray GL.$=
        GL.VertexArrayDescriptor 3
                                 GL.Float
                                 (fromIntegral $ vtxStride * szf)
                                 (nullPtr `plusPtr` 0)
    GL.arrayPointer GL.ColorArray GL.$=
        GL.VertexArrayDescriptor 4
                                 GL.Float
                                 (fromIntegral $ colStride * szf)
                                 (nullPtr `plusPtr` (colOffset * szf))
    GL.arrayPointer GL.TextureCoordArray GL.$=
        GL.VertexArrayDescriptor 2
                                 GL.Float
                                 (fromIntegral $ uvStride * szf)
                                 (nullPtr `plusPtr` (uvOffset * szf))
    mapM_ (\x -> GL.clientState x GL.$= GL.Enabled)
          [GL.VertexArray, GL.ColorArray, GL.TextureCoordArray]

    -- Draw
    GL.drawArrays GL.Quads 0 4

    -- Disable vertex arrays
    mapM_ (\x -> GL.clientState x GL.$= GL.Disabled)
          [GL.VertexArray, GL.ColorArray, GL.TextureCoordArray]

    -- Delete VAO / VBO
    --GL.deleteObjectNames [vao]
    GL.deleteObjectNames [vbo]

