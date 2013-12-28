
module StateModify ( modify'
                   , withDiscardStateT
                   ) where

import Control.Monad.State.Strict

-- Strict modify
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = get >>= (\x -> put $! f x)

-- Like withState, but discarding the state changes made by the passed action (like
-- ReaderT's local function)
withDiscardStateT :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withDiscardStateT f m = StateT $ \s -> runStateT m (f s) >>= \(a, _) -> return (a, s)

