
module LRUBoundedMap ( Map
                     , empty
                     , insert
                     , member
                     , notMember
                     , lookup
                     , delete
                     , update
                     ) where

import Prelude hiding (lookup)
import Data.Word

import qualified DoubleMap as DM
import qualified Data.Map.Strict as M

-- Bounded map maintaining a separate map for access history to drop the least
-- recently used element once the specified element limit is reached

-- TODO: Grand total of five O(log n) operations for insert and lookup, maybe we can do better?

data Map k v = Map (DM.Map k Word64 v) !Word64 !Int
                   deriving (Show)

empty :: Int -> Map k v
empty limit | limit >= 1 = Map DM.empty 0 limit
            | otherwise  = error "limit for LRUBoundedMap needs to be >= 1"

-- Insert a new element into the map, return the new map and the truncated
-- element (if over the limit)
insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe (k, v))
insert k v (Map m tick limit) =
    let inserted         = DM.insert k tick v m
        (truncE, truncM) = if   DM.size inserted > limit
                           then let (lruK, lruV) = snd $ M.findMin (snd $ DM.view inserted)
                                in  (Just (lruK, lruV), DM.delete (Left lruK) inserted)
                           else (Nothing, inserted)
    in  ( Map truncM (tick + 1) limit
        , truncE
        )

member :: Ord k => k -> Map k v -> Bool
member k (Map m _ _) = DM.member (Left k) m

notMember :: Ord k => k -> Map k v -> Bool
notMember k (Map m _ _) = DM.notMember (Left k) m

-- Lookup element, also update LRU time
lookup :: Ord k => k -> Map k v -> (Map k v, Maybe v)
lookup k bm@(Map m tick limit) = case DM.lookup (Left k) m of
    Just (_, kb, v) -> (Map (DM.updateKeyB kb tick m) (tick + 1) limit, Just v)
    Nothing         -> (bm, Nothing)

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m tick limit) = Map (DM.delete (Left k) m) tick limit

-- Update value, don't touch LRU time
update :: Ord k => k -> v -> Map k v -> Map k v
update k v (Map m tick limit) = Map (DM.update (Left k) v m) tick limit

