
{-# LANGUAGE   RecordWildCards
             , OverloadedStrings
             , LambdaCase
             , FlexibleContexts
             , BangPatterns #-}

module QuadRendering ( withQuadRenderer
                     , QuadRenderer
                     , withQuadRenderBuffer
                     , QuadRenderBuffer
                     , drawQuad
                     , QuadUV(..)
                       -- Re-exports from GLHelpers
                     , Transparency(..)
                       -- Re-exports from QuadRenderingAdHoc
                     , RGBA(..)
                     , FillColor(..)
                     , drawQuadImmediate
                     , drawQuadAdHocVBO
                     , drawQuadAdHocVBOShader
                     ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Mutable as VM
import Data.List
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import Trace
import GLHelpers
import Shaders
import QuadRenderingAdHoc

-- Module for efficient rendering of 2D quad primitives, used for UI elements and texture
-- mapped font rendering

data QuadRenderer = QuadRenderer
    { -- Vertex / Element Array Buffer Objects and layout
      qrVAO            :: !GL.VertexArrayObject
    , qrVBO            :: !GL.BufferObject
    , qrEBO            :: !GL.BufferObject
    , qrVtxStride      :: !Int
    , qrColStride      :: !Int
    , qrUVStride       :: !Int
    , qrTotalStride    :: !Int
    , qrMaxQuad        :: !Int
    , qrMaxVtx         :: !Int
    , qrMaxTri         :: !Int
    , qrVtxOffset      :: !Int
    , qrColOffset      :: !Int
    , qrUVOffset       :: !Int
      -- Shaders
    , qrShdProgTex     :: !GL.Program
    , qrShdProgColOnly :: !GL.Program
      -- TODO: Add some rendering statistics
    }

-- Initialize / clean up all OpenGL resources for our renderer
withQuadRenderer :: Int -> (QuadRenderer -> IO a) -> IO a
withQuadRenderer qrMaxQuad f = do
    traceOnGLError $ Just "withQuadRenderer begin"
    -- VAO
    qrVAO <- GL.genObjectName
    GL.bindVertexArrayObject GL.$= Just qrVAO
    -- VBO
    let szf           = sizeOf (0 :: Float)
        qrVtxStride   = 3
        qrColStride   = 4
        qrUVStride    = 2
        qrTotalStride = qrVtxStride + qrColStride + qrUVStride
        qrMaxTri      = qrMaxQuad * 2
        qrMaxVtx      = qrMaxTri * 4
        numfloat      = qrTotalStride * qrMaxVtx
        qrVtxOffset   = 0
        qrColOffset   = qrVtxOffset + qrVtxStride
        qrUVOffset    = qrColOffset + qrColStride
    qrVBO <- mkBindDynamicBO GL.ArrayBuffer $ numfloat * szf
    -- Specify and enable vertex attribute arrays
    vtxAttrib <- setAttribArray 0 qrVtxStride qrTotalStride qrVtxOffset
    colAttrib <- setAttribArray 1 qrColStride qrTotalStride qrColOffset
    uvAttrib  <- setAttribArray 2 qrUVStride  qrTotalStride qrUVOffset
    -- EBO
    let numidx = qrMaxTri * 3
        szi    = sizeOf(0 :: GL.GLuint)
    qrEBO <- mkBindDynamicBO GL.ElementArrayBuffer $ numidx * szi
    r <- runMaybeT $ do
        -- Create, compile and link shaders
        let mkShaderProgramMaybe vsSrc fsSrc =
                liftIO (mkShaderProgam vsSrc fsSrc) >>= \case
                    Left err   -> do
                        liftIO $ traceS TLError $ "withQuadRenderer - Shader error:\n " ++ err
                        mzero
                    Right prog -> liftIO $ do
                        -- Set shader attributes
                        GL.attribLocation prog "in_pos" GL.$= vtxAttrib
                        GL.attribLocation prog "in_col" GL.$= colAttrib
                        GL.attribLocation prog "in_uv"  GL.$= uvAttrib
                        return prog
        qrShdProgTex     <- mkShaderProgramMaybe vsSrcBasic fsSrcBasic
        qrShdProgColOnly <- mkShaderProgramMaybe vsSrcBasic fsColOnlySrcBasic
        -- Initialization done, run inner
        liftIO $ do
            disableVAOAndShaders
            traceOnGLError $ Just "withQuadRenderer begin inner"
            finally
                ( f $ QuadRenderer { .. } )
                ( do -- Cleanup
                     GL.deleteObjectName qrVAO
                     GL.deleteObjectNames [qrVBO, qrEBO]
                     mapM_ (GL.deleteObjectName) [qrShdProgTex, qrShdProgColOnly]
                     traceOnGLError $ Just "withQuadRenderer after cleanup"
                )
    -- Throw on error
    case r of
        Nothing -> error "withQuadRenderer - Shader init failed"
        Just r' -> return r'

-- TODO: Write an Unbox instance for this and switch to an unboxed mutable vector
data QuadRenderAttrib = QuadRenderAttrib
    { qaFillTransparency :: !Transparency
    , qaMaybeTexture     :: !(Maybe GL.TextureObject)
    , qaIndexIndex       :: !Int -- Index into the EBO so we know what primitive to render
                                 -- after sorting by attributes
    , qaDepth            :: !Float -- We store the depth / layer information already in the
                                   -- VBO, but replicate them here so we can sort for transparency
    } deriving (Eq)

-- Back-to-front ordering (transparency) and then sorting to reduce OpenGL state changes
--
-- TODO: Profiling shows we spend a good deal of time in compare. Since we're unlikely to
--       find a sort with less comparisons, maybe we can write a faster function here?
instance Ord QuadRenderAttrib where
    compare a b = let cmpDepth = compare (qaDepth            b) (qaDepth            a)
                      cmpTex   = compare (qaMaybeTexture     a) (qaMaybeTexture     b)
                      cmpTrans = compare (qaFillTransparency a) (qaFillTransparency b)
                  in  case () of
                          _ | cmpDepth /= EQ -> cmpDepth -- Sort by depth first
                            | cmpTex   /= EQ -> cmpTex   -- Sort by texture at the same depth
                            | otherwise      -> cmpTrans -- Finally by transparency

data QuadRenderBuffer = QuadRenderBuffer
    { qbVBOMap  :: !(VSM.IOVector GL.GLfloat      )
    , qbEBOMap  :: !(VSM.IOVector GL.GLuint       )
    , qbAttribs :: !(VM.IOVector  QuadRenderAttrib)
    , qbNumQuad :: !(IORef Int)
    , qbQR      :: !QuadRenderer
    }

-- Prepare data structures and render when inner exits. This is meant to be called once or
-- more per-frame. Runs its inner inside the base monad
withQuadRenderBuffer :: (MonadBaseControl IO m, MonadIO m)
                     => QuadRenderer
                     -> (QuadRenderBuffer -> m a)
                     -> m (Maybe a) -- We return Nothing if mapping fails
withQuadRenderBuffer qbQR@(QuadRenderer { .. }) f = do
    -- Map. If this function is nested inside a withQuadRenderBuffer with the same QuadRenderer,
    -- the mapping operation will fail as OpenGL does not allow two concurrent mappings. Hence,
    -- no need to check for this explicitly
    --
    -- TODO: Do not map and fill the EBO during inner / drawQuad, but rather inside
    --       drawRenderBuffer after material sorting. This way we can minimize the
    --       amount of draw calls
    --
    r <- control $ \run -> liftIO $
        let reportMappingFailure boType mf = do
                traceS TLError $
                    "withQuadRenderBuffer - " ++ boType ++ " mapping failure: " ++ show mf
                run $ return Nothing
            bindVAO = GL.bindVertexArrayObject GL.$= Just qrVAO
        in  bindVAO >> GL.withMappedBuffer -- VBO
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
                                           finally
                                               ( run $ do -- Run in outer base monad
                                                     let qb = QuadRenderBuffer { .. }
                                                     r <- f qb
                                                     return $ Just (r, qb)
                                               )
                                               bindVAO -- Make sure we rebind our VAO, otherwise
                                                       -- unmapping might fail if the inner
                                                       -- modified the bound buffer objects
                              )
                              ( reportMappingFailure "EBO" )
                )
                ( reportMappingFailure "VBO" )
    case r of
        Nothing       -> return Nothing
        Just (ra, qb) -> liftIO $ do
            -- Buffers have been successfully mapped, filled and unmapped, we can now render
            drawRenderBuffer qb
            return $ Just ra

-- Internal function to draw the contents of a render buffer once we're done filling it
drawRenderBuffer :: QuadRenderBuffer -> IO ()
drawRenderBuffer (QuadRenderBuffer { .. }) = do
    let QuadRenderer { .. } = qbQR
    GL.bindVertexArrayObject GL.$= Just qrVAO
    -- TODO: We're setting the shader matrix from the FFP projection matrix
    forM_ [qrShdProgTex, qrShdProgColOnly] $ \shdProg -> do
        GL.currentProgram GL.$= Just shdProg
        setProjMatrixFromFFP shdProg "in_mvp"
    -- Texture, use first TU
    GL.currentProgram  GL.$= Just qrShdProgTex
    (GL.get $ GL.uniformLocation qrShdProgTex "tex") >>= \loc ->
        GL.uniform loc GL.$= GL.Index1 (0 :: GL.GLint)
    GL.activeTexture   GL.$= GL.TextureUnit 0
    -- Sort attributes (transparency and reduce state changes)
    attribs <- readIORef qbNumQuad >>= \numQuad -> -- Number of used elements
               sort . V.toList                     -- TODO: Sort mutable vector in-place with
                                                   --       vector-algorithms?
               <$> ( V.unsafeFreeze                -- Can only convert immutable vector to a list
                         . VM.unsafeTake numQuad   -- Drop undefined elements
                         $ qbAttribs
                   )
    -- Setup some initial state and build corresponding attribute record
    GL.currentProgram              GL.$= Just qrShdProgColOnly
    GL.textureBinding GL.Texture2D GL.$= Nothing
    setTransparency TRNone
    let initialState = QuadRenderAttrib TRNone Nothing 0 0.0
    -- Draw all quads
    foldM_
        ( \oldA newA -> do
             -- Modify OpenGL state which changed between old / new rendering attributes
             case (qaMaybeTexture oldA, qaMaybeTexture newA) of
                (Just oldTex, Just newTex) ->
                    when (oldTex /= newTex) $
                        GL.textureBinding GL.Texture2D GL.$= Just newTex
                (Nothing, Just newTex) -> do
                    GL.currentProgram              GL.$= Just qrShdProgTex
                    GL.textureBinding GL.Texture2D GL.$= Just newTex
                (Just _, Nothing) ->
                    GL.currentProgram GL.$= Just qrShdProgColOnly
                (Nothing, Nothing) ->
                    return ()
             when (qaFillTransparency oldA /= qaFillTransparency newA) .
                 setTransparency $ qaFillTransparency newA
             -- Draw quad as two triangles
             let idxPerQuad = 2 * 3
                 szi        = sizeOf(0 :: GL.GLuint)
              in GL.drawElements GL.Triangles
                                 (fromIntegral idxPerQuad)
                                 GL.UnsignedInt
                                 $ nullPtr `plusPtr` (qaIndexIndex newA * idxPerQuad * szi)
             return $ newA
        )
        initialState
        attribs
    -- Done
    disableVAOAndShaders

data QuadUV = QuadUVDefault
            | QuadUV {-# UNPACK #-} !Float -- UV Bottom Left
                     {-# UNPACK #-} !Float
                     {-# UNPACK #-} !Float -- UV Top Right
                     {-# UNPACK #-} !Float
              deriving (Eq, Show)

-- Record all data to render the specified quad into the passed render buffer
drawQuad :: QuadRenderBuffer
         -> Float -> Float -> Float -> Float
         -> Float
         -> FillColor
         -> Transparency
         -> Maybe GL.TextureObject
         -> QuadUV
         -> IO ()
drawQuad (QuadRenderBuffer { .. })
         !x1 !y1 !x2 !y2
         !qaDepth
         col
         qaFillTransparency
         qaMaybeTexture
         uv = do
    let QuadRenderer { .. } = qbQR -- Bring buffer layout information into scope
    -- Are we at capacity?
    numQuad <- readIORef qbNumQuad
    if numQuad == qrMaxQuad
        then traceT TLError "drawQuad - QuadRenderBuffer overflow, dropping quad"
        else do
          -- Write vertex data to our mapped attribute buffers
          --
          -- TODO: Could use a hashmap to reuse vertices between quads
          --
          -- TODO: The code we're using is an unrolled version of this:
          --
          -- let (pos', cols, texs) = paramToPosColUV x1 y1 x2 y2 col
          --     vboOffs            = numQuad * 4
          -- forM_ (zip4 [vboOffs..] pos' cols texs) $
          --     \(i, (x, y), RGBA r g b a, (u, v)) ->
          --         forM_ (zip [0..] [x, y, (-qaDepth), r, g, b, a, u, v]) $
          --             \(offs, f) -> VSM.write qbVBOMap (i * qrTotalStride + offs) $ realToFrac f
          --
          -- Would be nice to find a more elegant yet still fast version
          --
          let !vtxBase = numQuad * 4 * qrTotalStride
              !vtx0    = vtxBase + (qrTotalStride * 0)
              !vtx1    = vtxBase + (qrTotalStride * 1)
              !vtx2    = vtxBase + (qrTotalStride * 2)
              !vtx3    = vtxBase + (qrTotalStride * 3)
              !( RGBA !r0 !g0 !b0 !a0
               , RGBA !r1 !g1 !b1 !a1
               , RGBA !r2 !g2 !b2 !a2
               , RGBA !r3 !g3 !b3 !a3
               ) = case col of FCWhite                 -> let c = RGBA 1 1 1 1 in (c, c, c, c)
                               FCBlack                 -> let c = RGBA 0 0 0 1 in (c, c, c, c)
                               FCSolid c               -> (c, c, c, c)
                               FCBottomTopGradient b t -> (b, b, t, t)
                               FCLeftRightGradient l r -> (l, r, l, r)
              !(!u0, !v0, !u1, !v1) =
                  case uv of QuadUVDefault          -> (0, 0, 1, 1)
                             QuadUV u0' v0' u1' v1' -> (u0', v0', u1', v1')
              uw      =  VSM.unsafeWrite qbVBOMap
           in do -- Vertex 0
                 uw (vtx0 + 0) $ realToFrac x1         -- X
                 uw (vtx0 + 1) $ realToFrac y1         -- Y
                 uw (vtx0 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx0 + 3) $ realToFrac r0         -- R
                 uw (vtx0 + 4) $ realToFrac g0         -- G
                 uw (vtx0 + 5) $ realToFrac b0         -- B
                 uw (vtx0 + 6) $ realToFrac a0         -- A
                 uw (vtx0 + 7) $ realToFrac u0         -- U
                 uw (vtx0 + 8) $ realToFrac v0         -- V
                 -- Vertex 1
                 uw (vtx1 + 0) $ realToFrac x2         -- X
                 uw (vtx1 + 1) $ realToFrac y1         -- Y
                 uw (vtx1 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx1 + 3) $ realToFrac r1         -- R
                 uw (vtx1 + 4) $ realToFrac g1         -- G
                 uw (vtx1 + 5) $ realToFrac b1         -- B
                 uw (vtx1 + 6) $ realToFrac a1         -- A
                 uw (vtx1 + 7) $ realToFrac u1         -- U
                 uw (vtx1 + 8) $ realToFrac v0         -- V
                 -- Vertex 2
                 uw (vtx2 + 0) $ realToFrac x2         -- X
                 uw (vtx2 + 1) $ realToFrac y2         -- Y
                 uw (vtx2 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx2 + 3) $ realToFrac r2         -- R
                 uw (vtx2 + 4) $ realToFrac g2         -- G
                 uw (vtx2 + 5) $ realToFrac b2         -- B
                 uw (vtx2 + 6) $ realToFrac a2         -- A
                 uw (vtx2 + 7) $ realToFrac u1         -- U
                 uw (vtx2 + 8) $ realToFrac v1         -- V
                 -- Vertex 3
                 uw (vtx3 + 0) $ realToFrac x1         -- X
                 uw (vtx3 + 1) $ realToFrac y2         -- Y
                 uw (vtx3 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx3 + 3) $ realToFrac r3         -- R
                 uw (vtx3 + 4) $ realToFrac g3         -- G
                 uw (vtx3 + 5) $ realToFrac b3         -- B
                 uw (vtx3 + 6) $ realToFrac a3         -- A
                 uw (vtx3 + 7) $ realToFrac u0         -- U
                 uw (vtx3 + 8) $ realToFrac v1         -- V
          -- Write index data to the mapped element array buffer
          let !eboOffs = numQuad * 6
              !vboOffs = numQuad * 4
              uw       = VSM.unsafeWrite qbEBOMap
           in -- Unrolled version of
              -- forM_ (zip [eboOffs..] [0, 1, 2, 0, 2, 3]) $ \(i, e) ->
              --     VSM.write qbEBOMap i (fromIntegral $ e + vboOffs)
              do uw (eboOffs + 0) . fromIntegral $ vboOffs + 0
                 uw (eboOffs + 1) . fromIntegral $ vboOffs + 1
                 uw (eboOffs + 2) . fromIntegral $ vboOffs + 2
                 uw (eboOffs + 3) . fromIntegral $ vboOffs + 0
                 uw (eboOffs + 4) . fromIntegral $ vboOffs + 2
                 uw (eboOffs + 5) . fromIntegral $ vboOffs + 3
          -- Write rendering attributes (need to be strict since it's not an unboxed vector)
          VM.unsafeWrite qbAttribs numQuad $! QuadRenderAttrib { qaIndexIndex = numQuad, .. }
          -- One more quad
          modifyIORef' qbNumQuad (+ 1)

