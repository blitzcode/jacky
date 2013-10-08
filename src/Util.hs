
module Util ( modify'
            ) where

import Control.Monad.State.Strict

-- Various unrelated bits used in several other modules

-- Strict modify
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = get >>= (\x -> put $! f x)

