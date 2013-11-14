
module StateModify ( modify'
                   , modifyM
                   ) where

import Control.Monad.State.Strict

-- Strict modify
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = get >>= (\x -> put $! f x)

modifyM :: MonadState s m => (s -> m (s, a)) -> m a
modifyM f = do
    s <- get
    (s', r) <- f s
    put $! s'
    return r

