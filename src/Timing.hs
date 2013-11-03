
module Timing ( getCurTick
              ) where

import Data.Time.Clock (getCurrentTime, utctDayTime)
-- import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

-- Timing functions for benchmarking

-- TODO: Look at various other timing packages for comparison:
--
-- http://hackage.haskell.org/packages/archive/repa-io/latest/doc/html/Data-Array-Repa-IO-Timing.html
-- http://hackage.haskell.org/packages/archive/timeit/1.0.0.0/doc/html/System-TimeIt.html

-- In seconds
getCurTick :: IO Double
getCurTick = do
    tickUCT <- getCurrentTime
    return (realToFrac $ utctDayTime tickUCT :: Double)
    --
    -- TODO: Compare with GLFW timer
    --
    -- Just time <- GLFW.getTime
    -- return time

