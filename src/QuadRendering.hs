
{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, FlexibleContexts #-}

module QuadRendering ( withQuadRenderer
                     , QuadRenderer
                     , withQuadRenderBuffer
                     , QuadRenderBuffer
                     , drawQuad
                     , RGBA(..)
                     , FillColor(..)
                     , FillTransparency(..)
                     , drawQuadImmediate
                     , drawQuadAdHocVBO
                     , drawQuadAdHocVBOShader
                     ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Mutable as VM
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import Trace
import GLImmediate
import Shaders

-- Module for efficient rendering of 2D quad primitives, used for UI elements and texture
-- mapped font rendering

data QuadRenderer = QuadRenderer
    { -- Vertex / Element Array Buffer Objects and layout
      qrVAO            :: !GL.VertexArrayObject
    , qrVBO            :: !GL.BufferObject
    , qrEBO            :: !GL.BufferObject
    , qrTotalStride    :: !Int
    , qrMaxQuad        :: !Int
    , qrMaxVtx         :: !Int
    , qrMaxTri         :: !Int
      -- Shaders
    , qrShdProgTex     :: !(Maybe GL.Program)
    , qrShdProgColOnly :: !(Maybe GL.Program)
    }

-- Initialize / clean up all OpenGL resources for our renderer
withQuadRenderer :: (QuadRenderer -> IO a) -> IO a
withQuadRenderer =
  bracket
    ( do -- VAO
         qrVAO <- GL.genObjectName
         GL.bindVertexArrayObject GL.$= Just qrVAO
         -- VBO
         let szf           = sizeOf (0 :: Float)
             vtxStride     = 3
             colStride     = 4
             uvStride      = 2
             qrTotalStride = vtxStride + colStride + uvStride
             qrMaxQuad     = 16384
             qrMaxTri      = qrMaxQuad * 2
             qrMaxVtx      = qrMaxTri * 4
             numfloat      = qrTotalStride * qrMaxVtx
             vtxOffset     = 0
             colOffset     = vtxOffset + vtxStride
             uvOffset      = colOffset + colStride
         qrVBO <- mkBindDynamicBO GL.ArrayBuffer $ numfloat * szf
         -- Specify and enable vertex attribute arrays
         vtxAttrib <- setAttribArray 0 vtxStride qrTotalStride vtxOffset
         colAttrib <- setAttribArray 1 colStride qrTotalStride colOffset
         uvAttrib  <- setAttribArray 2 uvStride  qrTotalStride uvOffset
         -- EBO
         let numidx = qrMaxTri * 3
             szi    = sizeOf(0 :: GL.GLuint)
         qrEBO <- mkBindDynamicBO GL.ElementArrayBuffer $ numidx * szi
         -- Create, compile and link shaders
         qrShdProgTex <- mkShaderProgam vsSrcBasic fsSrcBasic >>= \case
             Left  err -> do traceS TLError $ "withQuadRenderer - Shader error:\n " ++ err
                             return Nothing
             Right p   -> return $ Just p
         qrShdProgColOnly <- mkShaderProgam vsSrcBasic fsColOnlySrcBasic >>= \case
             Left  err -> do traceS TLError $ "withQuadRenderer - Shader error:\n " ++ err
                             return Nothing
             Right p   -> return $ Just p
         -- Set shader attributes
         forM_ [qrShdProgTex, qrShdProgColOnly] . maybe (return ()) $ \prog -> do
             GL.attribLocation prog "in_pos" GL.$= vtxAttrib
             GL.attribLocation prog "in_col" GL.$= colAttrib
             GL.attribLocation prog "in_uv"  GL.$= uvAttrib
         -- Done & fill in record
         disableVAOAndShaders
         return $ QuadRenderer { .. }
    )
    ( \QuadRenderer { .. } -> do GL.deleteObjectName qrVAO
                                 GL.deleteObjectNames [qrVBO, qrEBO]
                                 mapM_ (maybe (return ()) GL.deleteObjectName)
                                       [qrShdProgTex, qrShdProgColOnly]
    )

-- TODO: Write an Unbox instance for this and switch to an unboxed mutable vector
data QuadRenderAttrib = QuadRenderAttrib
    { -- TODO
    }

data QuadRenderBuffer = QuadRenderBuffer
    { qbVBOMap  :: !(VSM.IOVector GL.GLfloat      )
    , qbEBOMap  :: !(VSM.IOVector GL.GLuint       )
    , qbAttribs :: !(VM.IOVector  QuadRenderAttrib)
    , qbMaxQuad :: !Int
    , qbNumQuad :: !(IORef Int)
    }

-- Prepare data structures and render when inner exits. This is meant to be called once or
-- more per-frame. Runs its inner inside the base monad
withQuadRenderBuffer :: (MonadBaseControl IO m, MonadIO m)
                     => QuadRenderer
                     -> (QuadRenderBuffer -> m a)
                     -> m (Maybe a) -- We return Nothing if mapping fails
withQuadRenderBuffer (QuadRenderer { .. }) f = do
    -- Map. If this function is nested inside a withQuadRenderBuffer with the same QuadRenderer,
    -- the mapping operation will fail as OpenGL does not allow two concurrent mappings. Hence,
    -- no need to check for this explicitly 
    r <- control $ \run -> liftIO $
        let reportMappingFailure boType mf = do
                traceS TLError $
                    "withQuadRenderBuffer - " ++ boType ++
                    " mapping failure: " ++ show mf
                run $ return Nothing
        in  GL.withMappedBuffer -- VBO
            GL.ArrayBuffer
            GL.WriteOnly
            ( \ptrVBO -> newForeignPtr_ ptrVBO >>= \fpVBO ->
                  let numfloat = qrMaxVtx * qrTotalStride
                      qbVBOMap = VSM.unsafeFromForeignPtr0 fpVBO numfloat
                  in  GL.withMappedBuffer -- EBO
                          GL.ElementArrayBuffer
                          GL.WriteOnly
                          ( \ptrEBO -> newForeignPtr_ ptrEBO >>= \fpEBO ->
                                let numidx   = 3 * qrMaxTri
                                    qbEBOMap = VSM.unsafeFromForeignPtr0 fpEBO numidx
                                in  do qbNumQuad <- newIORef 0
                                       qbAttribs <- VM.new qrMaxQuad
                                       -- Run in outer base monad
                                       run $ Just <$>
                                           f QuadRenderBuffer { qbMaxQuad = qrMaxQuad , .. }
                          )
                          ( reportMappingFailure "EBO" )
            )
            ( reportMappingFailure "VBO" )
    case r of
        Nothing -> return Nothing
        Just ra -> do -- Buffers have been successfully mapped, filled and unmapped,
                      -- we can now render
                      --
                      -- TODO
                      --
                      return $ Just ra

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

-- Record all data to render the specified quad into the passed render buffer
drawQuad :: QuadRenderBuffer
         -> Float
         -> Float
         -> Float
         -> Float
         -> Float
         -> FillColor
         -> FillTransparency
         -> Maybe GL.TextureObject
         -> IO ()
drawQuad (QuadRenderBuffer { .. }) x1 y1 x2 y2 depth col trans tex = do
    --
    -- TODO
    --
    return ()

disableVAOAndShaders :: IO ()
disableVAOAndShaders = do
    -- Disable vertex attribute arrays and shaders
    GL.bindVertexArrayObject GL.$= Nothing
    GL.currentProgram        GL.$= Nothing

-- Create, bind and allocate Vertex / Element Array Buffer Object (VBO /  EBO)
mkBindDynamicBO :: GL.BufferTarget -> Int -> IO GL.BufferObject
mkBindDynamicBO target size = do
    vbo <- GL.genObjectName
    GL.bindBuffer target GL.$= Just vbo
    GL.bufferData target GL.$= ( fromIntegral size
                               , nullPtr
                               , GL.StreamDraw -- Dynamic
                               )
    return vbo

-- Convert rectangle position and draw parameters into a set of render-ready vertex attributes
paramToPosColUV :: Float
                -> Float
                -> Float
                -> Float
                -> FillColor
                -> ([(Float, Float)], [RGBA], [(Float, Float)])
paramToPosColUV x1 y1 x2 y2 col =
    let pos' = [ (x1, y1), (x2, y1), (x2, y2), (x1, y2) ]
        cols = case col of FCWhite                 -> replicate 4 (RGBA 1 1 1 1)
                           FCBlack                 -> replicate 4 (RGBA 0 0 0 1)
                           FCSolid c               -> replicate 4 c
                           FCBottomTopGradient b t -> [b, b, t, t]
                           FCLeftRightGradient l r -> [l, r, l, r]
        texs = [ (0, 0), (1, 0), (1, 1), (0, 1) ]
    in  (pos', cols, texs)

setTransparency :: FillTransparency -> IO ()
setTransparency trans =
    case trans of FTNone         -> GL.blend GL.$= GL.Disabled
                  FTBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat)
                  FTSrcAlpha     -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

-- Inefficient / obsolete (just used for testing / development) immediate mode and ad-hoc
-- drawing functions not actually using the QuadRenderer follow
--
-- Some references for getting basic rendering / shading like this up and running with
-- OpenGL in Haskell:
--
-- https://github.com/ocharles/blog/blob/master/code/2013-12-02-linear-example.hs
-- http://www.arcadianvisions.com/blog/?p=224
-- https://github.com/haskell-opengl/GLUT/blob/master/examples/Misc/SmoothOpenGL3.hs

drawQuadImmediate, drawQuadAdHocVBO, drawQuadAdHocVBOShader
    :: Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> FillColor
    -> FillTransparency
    -> Maybe GL.TextureObject
    -> IO ()

-- OpenGL 1.0 style immediate mode drawing
drawQuadImmediate x1 y1 x2 y2 depth col trans tex = do
    setTransparency trans
    setTextureFFP tex
    let (pos', cols, texs) = paramToPosColUV x1 y1 x2 y2 col
    -- Immediate mode drawing
    GL.renderPrimitive GL.Quads . forM_ (zip3 pos' cols texs) $
        \((x, y), RGBA r g b a, (u, v)) -> do
            color4f r g b a
            texCoord2f u v
            vertex3f x y (-depth)

-- OpenGL 1.5 style VBO + FFP drawing with ad-hoc buffer creation
drawQuadAdHocVBO x1 y1 x2 y2 depth col trans tex = do
    setTransparency trans
    setTextureFFP tex
    let (pos', cols, texs) = paramToPosColUV x1 y1 x2 y2 col
    -- VBO allocation
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
    vbo <- mkBindDynamicBO GL.ArrayBuffer $ totalSize * szf
    -- Fill mapped VBO with vertex data. We don't interleave the vertex attributes, but
    -- rather have the different arrays consecutively in the buffer
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
       ( \mf -> error $ "drawQuadAdHocVBO - OpenGL mapping failure: " ++ show mf )
    -- Specify and enable vertex arrays
    GL.arrayPointer GL.VertexArray GL.$=
        GL.VertexArrayDescriptor
            (fromIntegral vtxStride)
            GL.Float
            0
            (nullPtr `plusPtr` (vtxOffset * szf))
    GL.arrayPointer GL.ColorArray GL.$=
        GL.VertexArrayDescriptor
            (fromIntegral colStride)
            GL.Float
            0
            (nullPtr `plusPtr` (colOffset * szf))
    GL.arrayPointer GL.TextureCoordArray GL.$=
        GL.VertexArrayDescriptor
            (fromIntegral uvStride)
            GL.Float
            0
            (nullPtr `plusPtr` (uvOffset * szf))
    mapM_ (\x -> GL.clientState x GL.$= GL.Enabled)
          [GL.VertexArray, GL.ColorArray, GL.TextureCoordArray]
    -- Draw
    GL.drawArrays GL.Quads 0 $ fromIntegral numvtx
    -- Disable vertex arrays
    mapM_ (\x -> GL.clientState x GL.$= GL.Disabled)
          [GL.VertexArray, GL.ColorArray, GL.TextureCoordArray]
    -- Delete VBO
    GL.deleteObjectNames [vbo]

-- OpenGL 3.0 style VBA / VBO + shader drawing, also using an index buffer, interleaved
-- arrays and drawing triangles instead of quads. Buffers and shaders are created ad-hoc
drawQuadAdHocVBOShader x1 y1 x2 y2 depth col trans tex = do
    setTransparency trans
    let (pos', cols, texs) = paramToPosColUV x1 y1 x2 y2 col
    -- Create and bind Vertex Array Object (VAO)
    vao <- GL.genObjectName
    GL.bindVertexArrayObject GL.$= Just vao
    -- VBO allocation
    let szf         = sizeOf (0 :: Float)
        numvtx      = 4
        vtxStride   = 3
        colStride   = 4
        uvStride    = 2
        totalStride = vtxStride + colStride + uvStride
        totalSize   = totalStride * numvtx
        vtxOffset   = 0
        colOffset   = vtxOffset + vtxStride
        uvOffset    = colOffset + colStride
    vbo <- mkBindDynamicBO GL.ArrayBuffer $ totalSize * szf
    -- Fill mapped VBO with vertex data, interleaved attribute arrays
    GL.withMappedBuffer
       GL.ArrayBuffer
       GL.WriteOnly
       ( \ptr -> newForeignPtr_ ptr >>= \fp ->
             let vec = VSM.unsafeFromForeignPtr0 fp totalSize
             in  forM_ (zip4 [0..] pos' cols texs) $
                     \(i, (x, y), RGBA r g b a, (u, v)) ->
                         forM_ (zip [0..] [x, y, (-depth), r, g, b, a, u, v]) $
                             \(offs, f) -> VSM.write vec (i * totalStride + offs) f
       )
       ( \mf -> error $ "drawQuadAdHocVBOShader - VBO mapping failure: " ++ show mf )
    -- Specify and enable vertex attribute arrays
    vtxAttrib <- setAttribArray 0 vtxStride totalStride vtxOffset
    colAttrib <- setAttribArray 1 colStride totalStride colOffset
    uvAttrib  <- setAttribArray 2 uvStride  totalStride uvOffset
    -- Allocate element array (index) buffer object (EBO)
    let numidx = 2 * 3
        szi    = sizeOf(0 :: GL.GLuint)
    ebo <- mkBindDynamicBO GL.ElementArrayBuffer $ numidx * szi
    -- Fill mapped EBO with index data
    GL.withMappedBuffer
       GL.ElementArrayBuffer
       GL.WriteOnly
       ( \ptr -> newForeignPtr_ ptr >>= \fp ->
             let vec = VSM.unsafeFromForeignPtr0 fp numidx :: VSM.IOVector GL.GLuint
             in  forM_ (zip [0..] [0, 1, 2, 0, 2, 3]) $ \(i, e) -> VSM.write vec i e
       )
       ( \mf -> error $ "drawQuadAdHocVBOShader - EBO mapping failure: " ++ show mf )
    -- Create, compile and link shaders
    shdProg <- mkShaderProgam vsSrcBasic (if   isJust tex
                                          then fsSrcBasic
                                          else fsColOnlySrcBasic) >>= \case
        Left  err -> error $ "drawQuadAdHocVBOShader - Shader error:\n " ++ err
        Right p   -> return p
    -- Set shader attributes and activate
    GL.attribLocation shdProg "in_pos" GL.$= vtxAttrib
    GL.attribLocation shdProg "in_col" GL.$= colAttrib
    GL.attribLocation shdProg "in_uv"  GL.$= uvAttrib
    GL.currentProgram GL.$= Just shdProg
    -- Projection matrix
    {-
    let l = 0
        r = 100
        b = 0
        t = 100
        n = 0
        f = 1000
        projection = VS.fromList [ 2 / (r - l), 0, 0, 0,
                                   0, 2 / (t - b), 0, 0,
                                   0, 0, -2 / (f - n), 0,
                                   -(r + l) / (r - l), -(t + b) / (t - b), -(f + n) / (f - n), 1
                                 ] :: VS.Vector GL.GLfloat
    let projection = VS.fromList [ 1 / 1280, 0, 0, 0
                                 , 0, 1 / 720, 0, 0
                                 , 0, 0, 1 / 1000, 0
                                 , 0, 0, 0, 1
                                 ] :: VS.Vector GL.GLfloat
    -}
    -- TODO: We're setting the shader matrix from FFP projection matrix
    setProjMatrixFromFFP shdProg "in_mvp"
    -- Textures
    when (isJust tex) $
        setTextureShader (fromJust tex) 0 shdProg "tex"
    -- Draw quad as two triangles
    GL.drawElements GL.Triangles (fromIntegral numidx) GL.UnsignedInt nullPtr
    -- Delete shaders / EBO / VBO / VAO
    disableVAOAndShaders
    GL.deleteObjectName shdProg
    GL.deleteObjectName ebo
    GL.deleteObjectName vbo
    GL.deleteObjectName vao

setTextureFFP :: Maybe GL.TextureObject -> IO ()
setTextureFFP tex = do
    -- FFP texture setup
    case tex of
        Just _ -> do
            GL.texture        GL.Texture2D GL.$= GL.Enabled
            GL.textureBinding GL.Texture2D GL.$= tex
        Nothing ->
            GL.texture        GL.Texture2D GL.$= GL.Disabled

