
module BoundedMap ( BoundedMap
                  , mkBoundedMap
                  , insert
                  , update
                  , pop
                  , view
                  ) where

import qualified BoundedStack as BS
import qualified Data.Map.Strict as M

-- Bounded map maintaining a FIFO to drop the oldest element when its specified
-- element limit is reached

data BoundedMap k v = BoundedMap (BS.BoundedStack k) (M.Map k v)
                      deriving (Show)

mkBoundedMap :: Int -> BoundedMap k v
mkBoundedMap limit | limit >= 1 = BoundedMap (BS.mkBoundedStack limit) M.empty
                   | otherwise  = error "limit for BoundedMap needs to be >= 1"

-- Insert a new element into the map, return the new map and the truncated
-- element (if over the limit)
insert :: Ord k => k -> v -> BoundedMap k v -> (BoundedMap k v, Maybe (k, v))
insert k v (BoundedMap st m) =
    let isNew              = M.notMember k m
        (newStack, newMap) = if   isNew
                             then (BS.push k st, M.insert k v m)
                             else ((st, Nothing), m)
        lookupE k'         = case M.lookup k' m of
                                 Just v' -> v'
                                 Nothing -> error $ "BoundedMap.insertBoundedMap: "
                                                    ++ "Key in FIFO but not in Map"
    in  case newStack of
            (st', Nothing    ) -> ( BoundedMap st' newMap
                                  , Nothing
                                  )
            (st', Just kTrunc) -> ( BoundedMap st' $ M.delete kTrunc newMap
                                  , Just (kTrunc, lookupE kTrunc)
                                  )

-- Update an existing element (does nothing if the element is not found)
update :: Ord k => k -> v -> BoundedMap k v -> BoundedMap k v
update k v (BoundedMap st m) =
    let m' = M.update (\_ -> Just v) k m
    in  BoundedMap st m'

-- LIFO pop
pop :: Ord k => BoundedMap k v -> (Maybe (k, v), BoundedMap k v)
pop (BoundedMap st m) =
    let (k, st')   = BS.pop st
        lookupE k' = case M.lookup k' m of
                         Just v' -> v'
                         Nothing -> error $ "BoundedMap.popBoundedMap: "
                                            ++ "Key in FIFO but not in Map"
    in  case k of Just trunc -> (Just (trunc, lookupE trunc), BoundedMap st' (M.delete trunc m))
                  Nothing    -> (Nothing, BoundedMap st m)

view :: BoundedMap k v -> M.Map k v
view (BoundedMap _ m) = m

