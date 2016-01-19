
module StateModify ( withDiscardStateT
                   ) where

import Control.Monad.State.Strict

-- Like withState, but discarding the state changes made by the passed action (like
-- ReaderT's local function)
withDiscardStateT :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withDiscardStateT f m = StateT $ \s -> runStateT m (f s) >>= \(a, _) -> return (a, s)

