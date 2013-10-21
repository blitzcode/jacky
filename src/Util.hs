
module Util ( modify'
            , parseMaybeInt
            ) where

import Control.Monad.State.Strict
import Control.Applicative
import Data.Maybe

-- Various unrelated bits used in several other modules

-- Strict modify
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = get >>= (\x -> put $! f x)

parseMaybeInt :: String -> Maybe Int
parseMaybeInt s = fst <$> (listToMaybe . reads) s

