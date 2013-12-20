
{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}

module QuadRendering ( withQuadRenderer
                     , QuadRenderer
                     , RGBA(..)
                     , FillColor(..)
                     , FillTransparency(..)
                     , drawQuadImmediate
                     , drawQuadAdHocVBO
                     , drawQuadAdHocVBOShader
                     ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Data.Vector.Storable.Mutable as VSM
import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Exception
import Control.Monad
import Control.Monad.Error
-- import Control.Applicative
-- import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import GLImmediate

-- Module for efficient rendering of 2D quad primitives, used for UI elements and texture
-- mapped font rendering

data QuadRenderer = QuadRenderer { qrVtxBuf :: !GL.BufferObject
                                 , qrColBuf :: !GL.BufferObject
                                 , qrUVBuf  :: !GL.BufferObject
                                 }

withQuadRenderer :: (QuadRenderer -> IO a) -> IO a
withQuadRenderer =
    bracket ( do [qrVtxBuf, qrColBuf, qrUVBuf] <- GL.genObjectNames 3
                 return $ QuadRenderer { .. }
            )
            ( \QuadRenderer { .. } -> GL.deleteObjectNames [qrVtxBuf, qrColBuf, qrUVBuf] )

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
    vbo <- mkBindDynamicVBO $ totalSize * szf
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
    vbo <- mkBindDynamicVBO $ totalSize * szf
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
    let vtxAttrib = GL.AttribLocation 0
        colAttrib = GL.AttribLocation 1
        uvAttrib  = GL.AttribLocation 2
    GL.vertexAttribPointer vtxAttrib GL.$=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor
              (fromIntegral vtxStride)
              GL.Float
              (fromIntegral $ totalStride * szf)
              (nullPtr `plusPtr` (vtxOffset * szf))
        )
    GL.vertexAttribPointer colAttrib GL.$=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor
              (fromIntegral colStride)
              GL.Float
              (fromIntegral $ totalStride * szf)
              (nullPtr `plusPtr` (colOffset * szf))
        )
    GL.vertexAttribPointer uvAttrib GL.$=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor
              (fromIntegral uvStride)
              GL.Float
              (fromIntegral $ totalStride * szf)
              (nullPtr `plusPtr` (uvOffset * szf))
        )
    mapM_ (\x -> GL.vertexAttribArray x GL.$= GL.Enabled) [vtxAttrib, colAttrib, uvAttrib]
    -- Allocate element array (index) buffer object (EBO)
    let numidx = 2 * 3
        szi    = sizeOf(0 :: GL.GLuint)
    ebo <- GL.genObjectName
    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ebo
    GL.bufferData GL.ElementArrayBuffer GL.$= ( fromIntegral $ numidx * szi
                                              , nullPtr
                                              , GL.StreamDraw -- Dynamic
                                              )
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
    let vsSrc = TE.encodeUtf8 $ T.unlines
            [ "#version 120"
            , "uniform mat4 in_mvp;"
            , "attribute vec3 in_pos;"
            , "attribute vec4 in_col;"
            , "attribute vec2 in_uv;"
            , "varying vec4 fs_col;"
            , "varying vec2 fs_uv;"
            , "void main()"
            , "{"
            , "    gl_Position = in_mvp * vec4(in_pos, 1.0);"
            , "    fs_col      = in_col;"
            , "    fs_uv       = in_uv;"
            , "}"
            ]
        fsSrc = TE.encodeUtf8 $ T.unlines
            [ "#version 120"
            , "varying vec4 fs_col;"
            , "varying vec2 fs_uv;"
            , "uniform sampler2D tex;"
            , "void main()"
            , "{"
            , "   gl_FragColor = fs_col * texture2D(tex, fs_uv);"
            , "}"
            ]
        fsColOnlySrc = TE.encodeUtf8 $ T.unlines
            [ "#version 120"
            , "varying vec4 fs_col;"
            , "void main()"
            , "{"
            , "   gl_FragColor = fs_col;"
            , "}"
            ]
    shdProg <- mkShaderProgam vsSrc (if isJust tex then fsSrc else fsColOnlySrc) >>= \case
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
    GL.UniformLocation loc <- GL.get $ GL.uniformLocation shdProg "in_mvp"
    withArray ([1..16] :: [GL.GLfloat]) $ \ptr -> do GLR.glGetFloatv GLR.gl_PROJECTION_MATRIX ptr
                                                     GLR.glUniformMatrix4fv loc 1 0 ptr
    -- Textures
    when (isJust tex) $
        setTextureShader (fromJust tex) 0 shdProg "tex"
    -- Draw quad as two triangles
    GL.drawElements GL.Triangles (fromIntegral numidx) GL.UnsignedInt nullPtr
    -- Disable vertex attribute arrays and shaders
    GL.bindVertexArrayObject GL.$= Nothing
    GL.currentProgram        GL.$= Nothing
    -- Delete shaders / EBO / VBO / VAO
    GL.deleteObjectName shdProg
    GL.deleteObjectName ebo
    GL.deleteObjectName vbo
    GL.deleteObjectName vao

mkShaderProgam :: B.ByteString -> B.ByteString -> IO (Either String GL.Program)
mkShaderProgam vsSrc fsSrc =
    -- Always delete the shaders (don't need them after linking), only delete the program
    -- on error
    bracket        (GL.createShader GL.VertexShader  ) (GL.deleteObjectName) $ \shdVtx  ->
    bracket        (GL.createShader GL.FragmentShader) (GL.deleteObjectName) $ \shdFrag ->
    bracketOnError (GL.createProgram                 ) (GL.deleteObjectName) $ \shdProg -> do
        r <- runErrorT $ do
                 compile shdVtx  vsSrc
                 compile shdFrag fsSrc
                 liftIO $ GL.attachShader shdProg shdVtx >> GL.attachShader shdProg shdFrag
                 link shdProg
                 liftIO $ GL.detachShader shdProg shdVtx >> GL.detachShader shdProg shdFrag
                 return shdProg
        -- The bracket only deletes in case of an exception, still need to delete manually
        -- in case of a monadic error
        when (null $ rights [r]) $ GL.deleteObjectName shdProg
        return r
    -- Compile and link helpers
    where compile shd src = do
              liftIO $ do GL.shaderSourceBS shd GL.$= src
                          GL.compileShader  shd
              success <- liftIO $ GL.get $ GL.compileStatus shd
              unless success $ do
                  errLog <- liftIO $ GL.get $ GL.shaderInfoLog shd
                  throwError errLog
          link prog = do
              liftIO $ GL.linkProgram prog
              success <- liftIO $ GL.get $ GL.linkStatus prog
              unless success $ do
                  errLog <- liftIO $ GL.get $ GL.programInfoLog prog
                  throwError errLog

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

setTextureShader :: GL.TextureObject -> Int -> GL.Program -> String -> IO ()
setTextureShader tex tu prog uname = do
    (GL.get $ GL.uniformLocation prog uname) >>= \loc ->
        GL.uniform loc             GL.$= GL.Index1 (fromIntegral tu :: GL.GLint)
    GL.activeTexture               GL.$= GL.TextureUnit (fromIntegral tu)
    GL.textureBinding GL.Texture2D GL.$= Just tex

setTextureFFP :: Maybe GL.TextureObject -> IO ()
setTextureFFP tex = do
    -- FFP texture setup
    case tex of
        Just _ -> do
            GL.texture        GL.Texture2D GL.$= GL.Enabled
            GL.textureBinding GL.Texture2D GL.$= tex
        Nothing ->
            GL.texture        GL.Texture2D GL.$= GL.Disabled

-- Create, bind and allocate Vertex Buffer Object (VBO)
mkBindDynamicVBO :: Int -> IO GL.BufferObject
mkBindDynamicVBO size = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
    GL.bufferData GL.ArrayBuffer GL.$= ( fromIntegral size
                                       , nullPtr
                                       , GL.StreamDraw -- Dynamic
                                       )
    return vbo

