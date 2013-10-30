
module Util ( modify'
            , modifyM
            , parseMaybeInt
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

parseMaybeInt :: String -> Maybe Int
parseMaybeInt s = fst <$> (listToMaybe . reads) s

