
module Timing ( getTick
              , timeIt
              ) where

import Data.Time.Clock
import Control.Applicative
import Control.Monad.IO.Class
import System.IO.Unsafe

-- Timing functions

-- TODO: Consider just using the criterion package for all performance measurements
--       http://hackage.haskell.org/package/criterion

{-# NOINLINE startTime #-}
startTime :: UTCTime
startTime = unsafePerformIO getCurrentTime

-- In seconds
getTick :: IO Double
getTick =
    realToFrac <$> flip diffUTCTime startTime <$> getCurrentTime

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
    start <- liftIO getCurrentTime
    r     <- f
    end   <- liftIO getCurrentTime
    return (realToFrac $ diffUTCTime end start, r)

