
module Timing ( getCurTick
              , timeIt
              ) where

import Data.Time.Clock
import Control.Applicative
import Control.Monad.IO.Class

-- Timing functions for benchmarking

-- TODO: Consider just using the criterion package for all if this
--       http://hackage.haskell.org/package/criterion

-- In seconds
getCurTick :: IO Double
getCurTick = do
    realToFrac <$> utctDayTime <$> getCurrentTime -- TODO: This should wrap around at midnight...

    -- TODO: Compare with GLFW timer
    --
    -- {-# LANGUAGE PackageImports #-}
    --
    -- import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
    --
    -- Just time <- GLFW.getTime
    -- return time

timeIt :: MonadIO m => m a -> m (Double, a)
timeIt f = do
    start <- liftIO $ getCurrentTime
    r     <- f
    end   <- liftIO $ getCurrentTime
    return (realToFrac $ diffUTCTime end start, r)

