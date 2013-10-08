
{-# LANGUAGE PackageImports #-}

module GLHelpers ( GLFWEvent(..)
                 , errorCallback
                 , keyCallback
                 , withWindow
                 , setup2DOpenGL
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW -- Be explicit, we need the newer GLFW-b
import Control.Concurrent.STM
import Control.Monad.IO.Class

-- Various utility functions related to GL and GLFW

-- Convert GLFW callbacks into events delivered to a queue

data GLFWEvent = GLFWEventError GLFW.Error String
               | GLFWEventKey   GLFW.Window GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys

errorCallback :: TQueue GLFWEvent -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically . writeTQueue tc $ GLFWEventError e s

keyCallback :: TQueue GLFWEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState ->
               GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically . writeTQueue tc $ GLFWEventKey win k sc ka mk

withWindow :: (MonadIO m) => Int -> Int -> String -> (GLFW.Window -> m ()) -> m ()
withWindow w h title f = do
    -- TODO: Use bracket
    liftIO $ do
        GLFW.setErrorCallback $ Just simpleErrorCallback
        True <- GLFW.init 
        GLFW.windowHint $ GLFW.WindowHint'Resizable False
    Just window <- liftIO $ GLFW.createWindow w h title Nothing Nothing
    liftIO . GLFW.makeContextCurrent $ Just window
    f window
    liftIO $ do
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow window
        GLFW.terminate
    where
        simpleErrorCallback e s = putStrLn $ show e ++ " " ++  show s

setup2DOpenGL :: Int -> Int -> IO ()
setup2DOpenGL w h = do
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)
    -- Magic number working for NV & ATI
    GL.translate (GL.Vector3 0.375 0.375 0.0 :: GL.Vector3 GL.GLfloat)

