
{-# LANGUAGE PackageImports, LambdaCase #-}

module GLHelpers ( GLFWEvent(..)
                 , withWindow
                 , setup2DOpenGL
                 , getCurTex2DSize
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW -- Be explicit, we need the newer GLFW-b
import Control.Concurrent.STM
import Control.Exception
import Control.Applicative
import Text.Printf

import Trace

-- Various utility functions related to GL and GLFW

-- Convert GLFW callbacks into events delivered to a queue

data GLFWEvent = GLFWEventError      GLFW.Error String
               | GLFWEventKey        GLFW.Window GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
               | GLFWEventWindowSize GLFW.Window Int Int

errorCallback :: TQueue GLFWEvent -> GLFW.Error -> String -> IO ()
errorCallback tq e s = atomically . writeTQueue tq $ GLFWEventError e s

keyCallback :: TQueue GLFWEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState ->
               GLFW.ModifierKeys -> IO ()
keyCallback tq win k sc ka mk = atomically . writeTQueue tq $ GLFWEventKey win k sc ka mk

windowSizeCallback :: TQueue GLFWEvent -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback tq win w h = atomically . writeTQueue tq $ GLFWEventWindowSize win w h

withWindow :: Int -> Int -> String -> TQueue GLFWEvent -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title tq f =
    bracket
        ( do
             GLFW.setErrorCallback . Just $ errorCallback tq
             True <- GLFW.init 
             GLFW.windowHint $ GLFW.WindowHint'Resizable True
             -- GLFW.windowHint $ GLFW.WindowHint'Samples 4 -- 4x anti-aliasing
             Just window <- GLFW.createWindow w h title Nothing Nothing
             GLFW.setKeyCallback        window . Just $ keyCallback        tq
             GLFW.setWindowSizeCallback window . Just $ windowSizeCallback tq
             GLFW.makeContextCurrent $ Just window
             traceS TLInfo =<< getStrings
             -- traceS TLInfo =<< (show <$> GL.get GL.glExtensions)
             return window
        )
        ( \window -> do GLFW.destroyWindow window
                        GLFW.terminate
        )
        f

setup2DOpenGL :: Int -> Int -> IO ()
setup2DOpenGL w h = do
    GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)
    -- GL.ortho 0.0 (fromIntegral w) 0.0 (fromIntegral h) 0.1 1000.0
    -- Magic number working for NV & ATI
    GL.translate (GL.Vector3 0.375 0.375 0.0 :: GL.Vector3 GL.GLfloat)

getCurTex2DSize :: IO (Int, Int)
getCurTex2DSize = (\case (GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
                         <$> (GL.get $ GL.textureSize2D (Left GL.Texture2D) 0)

getStrings :: IO String
getStrings =
    printf "OpenGL - Vendor: %s · Renderer: %s · Version: %s · GLSL: %s · Num Extensions: %i"
           <$> GL.get GL.vendor
           <*> GL.get GL.renderer
           <*> GL.get GL.glVersion
           <*> GL.get GL.shadingLanguageVersion
           <*> (length <$> GL.get GL.glExtensions)

