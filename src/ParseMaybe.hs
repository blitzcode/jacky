
module ParseMaybe ( parseMaybe
                  ) where

import Control.Applicative
import Data.Maybe

parseMaybe :: Read a => String -> Maybe a
parseMaybe s = fst <$> (listToMaybe . reads) s

