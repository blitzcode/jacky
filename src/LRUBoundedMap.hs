
module LRUBoundedMap ( Map
                     , empty
                     , insert
                     , update
                     , member
                     , notMember
                     , lookup
                     , delete
                     , pop
                     , size
                     , view
                     , valid
                     ) where

import qualified Data.HashMap.Strict as HM
import Prelude hiding (lookup, last)
import Control.Monad.Writer
import Data.Hashable
import Data.Maybe

-- Map dropping least recently used item when growing over a specified limit
--
-- Implementation based on Data.Cache.LRU / lrucache, main difference is basing
-- the code on HashMap instead of Map and the insert function returning the
-- truncated element.
--
-- TODO: Map.size is O(1) while HashMap.size is O(n), maybe cache the size?
--
-- TODO: Even though the linked-list implementation allows us to use an
--       unordered container, this is still not much faster than the previous
--       version of this code based on DoubleMap

data Map k v = Map { mFirst :: !(Maybe k)
                   , mLast  :: !(Maybe k)
                   , mLimit :: !Int
                   , mMap   :: !(HM.HashMap k (Link k v))
                   }

data Link k v = Link { lPrev :: !(Maybe k)
                     , lNext :: !(Maybe k)
                     , lVal  :: v
                     }

empty :: Int -> Map k v
empty limit | limit >= 1 = Map { mFirst = Nothing
                               , mLast  = Nothing
                               , mLimit = limit
                               , mMap   = HM.empty
                               }
            | otherwise  = error "limit for LRUBoundedMap needs to be >= 1"

size :: Map k v -> (Int, Int)
size (Map _ _ limit content) = (HM.size content, limit)

member :: (Eq k, Hashable k) => k -> Map k v -> Bool
member k = HM.member k . mMap

notMember :: (Eq k, Hashable k) => k -> Map k v -> Bool
notMember k = not . HM.member k . mMap

view :: Map k v -> [(k, v)]
view = map (\(k, lnk) -> (k, lVal lnk)) . HM.toList . mMap

-- Lookup element, also update LRU
lookup :: (Eq k, Hashable k) => k -> Map k v -> (Map k v, Maybe v)
lookup k m =
    case HM.lookup k $ mMap m of
        Nothing  -> (m, Nothing)
        Just lnk -> (hit k m, Just $ lVal lnk)

-- Move the passed key to the front of the list (most recently used). Note that this
-- function assumes the key is actually in the map
{-# INLINE hit #-}
hit :: (Eq k, Hashable k) => k -> Map k v -> Map k v
hit k m@(Map first last limit content) =
    let Just firstK   = first
        Just lastK    = last
        Just lastLnk  = HM.lookup lastK content
        adjFront      = HM.adjust (\v -> v { lPrev = Just k }) firstK .
                        HM.adjust (\v -> v { lPrev = Nothing
                                           , lNext = first
                                           }
                                  ) k
        Just prevLast = lPrev lastLnk
        Just kL       = HM.lookup k content
        Just prevK    = lPrev kL
        Just nextK    = lNext kL
    in  case () of _ | k == firstK -> m -- Already at the front
                     | k == lastK  -> -- Move up last
                                      Map (Just k)
                                          (lPrev lastLnk)
                                          limit
                                          -- Second last now last, having no next
                                          . HM.adjust (\v -> v { lNext = Nothing })
                                                      prevLast
                                                      . adjFront $ content -- Update the new first
                     | otherwise   -> -- Move to front from the middle
                                      Map (Just k)
                                          last
                                          limit
                                          -- Remove key from the middle
                                          . HM.adjust (\v -> v { lNext = Just nextK }) prevK
                                          . HM.adjust (\v -> v { lPrev = Just prevK }) nextK
                                          . adjFront $ content -- Update the new first

delete :: (Eq k, Hashable k) => k -> Map k v -> (Map k v, Maybe v)
delete k m@(Map first last limit content) =
    let Just firstK    = first
        Just lastK     = last
        Just nextK     = lNext kL
        Just prevK     = lPrev kL
        (deleted, mKL) = -- TODO: Map had updateLookupWithKey, now we need 2x O(log n)
                         case HM.lookup k $ mMap m of
                             Just v  -> (HM.delete k content, Just v)
                             Nothing -> (content, Nothing)
        Just kL        = mKL
        mKLV           = Just $ lVal kL
    in  case () of _ | isNothing mKL        -> -- Key not in map
                                               (m, Nothing)
                     | HM.size content == 1 -> -- Just drop the remaining item
                                               ( Map Nothing Nothing limit deleted
                                               , mKLV
                                               )
                     | k == firstK          -> -- Remove first
                                               ( Map (lNext kL)
                                                      last
                                                      limit
                                                      . HM.adjust (\v -> v { lPrev = Nothing })
                                                                  nextK
                                                                  $ deleted
                                               , mKLV
                                               )
                     | k == lastK           -> -- Remove last
                                               ( Map first
                                                     (lPrev kL)
                                                     limit
                                                     . HM.adjust (\v -> v { lNext = Nothing })
                                                                 prevK
                                                                 $ deleted
                                               , mKLV
                                               )
                                               
                     | otherwise            -> -- Remove from the middle, first / last unchanged
                                               ( Map first
                                                     last
                                                     limit
                                                     . HM.adjust
                                                           (\v -> v { lNext = lNext kL }) prevK
                                                     . HM.adjust
                                                           (\v -> v { lPrev = lPrev kL }) nextK
                                                     $ deleted
                                               , mKLV
                                               )

-- Delete and return most recently used item
pop :: (Eq k, Hashable k) => Map k v -> (Map k v, Maybe (k, v))
pop m =
    if   (fst $ size m) == 0
    then (m, Nothing)
    else let (m', Just v) = delete first m
             Just first   = mFirst m
         in  (m', Just (first, v))

update :: (Eq k, Hashable k) => k -> v -> Map k v -> Map k v
update k v m =
    case insertInternal True k v m of
        (m', Nothing) -> m'
        _             -> error "LRUBoundedMap.update: insertInternal truncated with updateOnly"

-- Insert a new element into the map, return the new map and the truncated
-- element (if over the limit)
insert ::  (Eq k, Hashable k) => k -> v -> Map k v -> (Map k v, Maybe (k, v))
insert = insertInternal False

insertInternal :: (Eq k, Hashable k) => Bool -> k -> v -> Map k v -> (Map k v, Maybe (k, v))
insertInternal updateOnly k v m@(Map first last limit content) =
    let insertEmpty  = Map (Just k)
                           (Just k)
                           limit
                           (HM.insert k (Link Nothing Nothing v) content)
        insertUpdate = ( hit k $ m { mMap = HM.adjust (\v' -> v' { lVal = v }) k content }
                       , Nothing
                       )
        insertAdd    = if   HM.size content == limit
                       then addFull
                       else (add, Nothing)
        -- Add to the front
        inserted     = HM.insert k firstL
                           . HM.adjust (\v' -> v' { lPrev = Just k })
                                       firstK
                                       $ content
        add          = m { mFirst = Just k
                         , mMap   = inserted
                         }
        Just firstK  = first
        firstL       = Link Nothing (Just firstK) v
        -- Delete last
        addFull      = case delete lastK add of (m', Nothing) -> (m', Nothing         )
                                                (m', Just v') -> (m', Just (lastK, v'))
        Just lastK   = last
    -- We can have an empty or a non-empty list, the item can be already in the
    -- map or not, and we can be in insert or update mode, handle all cases below
    in  case () of _ | HM.null content && (not updateOnly) -> (insertEmpty, Nothing)
                     | HM.member k content                 -> insertUpdate
                     | not updateOnly                      -> insertAdd
                     | otherwise                           -> (m, Nothing)

valid :: (Eq k, Hashable k) => Map k v -> Maybe String
valid (Map first last limit content) =
    let w = execWriter $ do
                when (limit < 1)               $ tell "limit < 1\n"
                when (HM.size content > limit) $ tell "Size over the limit\n"
                when (length keysForwards /= HM.size content) $
                    tell "Map / linked-list size mismatch\n"
                when (keysForwards /= reverse keysBackwards) $
                    tell "Forwards and backwards traversal gives different lists\n"
                when (not $ all (`HM.member` content) keysForwards) $
                    tell "Not all keys from the linked-list present in the map\n"
        keysForwards           = traverse (lNext) first
        keysBackwards          = traverse (lPrev) last
        traverse _ Nothing     = []
        traverse step (Just k) = let Just nextK = HM.lookup k content
                                 in  k : (traverse (step) $ step nextK)
    in  case w of [] -> Nothing
                  xs -> Just xs

