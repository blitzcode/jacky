
module Util ( modify'
            , modifyM
            , parseMaybe
            ) where

import Control.Monad.State.Strict
import Control.Applicative
import Data.Maybe

-- Various unrelated bits used in several other modules

-- Strict modify
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = get >>= (\x -> put $! f x)

modifyM :: MonadState s m => (s -> m (s, a)) -> m a
modifyM f = do
    s <- get
    (s', r) <- f s
    put $! s'
    return r

parseMaybe :: Read a => String -> Maybe a
parseMaybe s = fst <$> (listToMaybe . reads) s

