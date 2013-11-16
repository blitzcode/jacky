
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

import qualified Data.Map.Strict as M
import Prelude hiding (lookup, last)
import Control.Monad.Writer

-- Map dropping least recently used item when growing over a specified limit
--
-- Implementation based on Data.Cache.LRU / lrucache

data Map k v = Map { mFirst :: !(Maybe k)
                   , mLast  :: !(Maybe k)
                   , mLimit :: !Int
                   , mMap   :: !(M.Map k (Link k v))
                   }

data Link k v = Link { lPrev :: !(Maybe k)
                     , lNext :: !(Maybe k)
                     , lVal  :: !v
                     }

empty :: Int -> Map k v
empty limit | limit >= 1 = Map { mFirst = Nothing
                               , mLast  = Nothing
                               , mLimit = limit
                               , mMap   = M.empty
                               }
            | otherwise  = error "limit for LRUBoundedMap needs to be >= 1"

size :: Map k v -> (Int, Int)
size (Map _ _ limit content) = (M.size content, limit)

member :: Ord k => k -> Map k v -> Bool
member k = M.member k . mMap

notMember :: Ord k => k -> Map k v -> Bool
notMember k = M.notMember k . mMap

view :: Map k v -> [(k, v)]
view = map (\(k, lnk) -> (k, lVal lnk)) . M.toList . mMap

-- Lookup element, also update LRU
lookup :: Ord k => k -> Map k v -> (Map k v, Maybe v)
lookup k m =
    case M.lookup k $ mMap m of
        Nothing  -> (m, Nothing)
        Just lnk -> (hit k m, Just $ lVal lnk)

-- Move the passed key to the front of the list (most recently used). Note that this
-- function assumes the key is actually in the map
hit :: Ord k => k -> Map k v -> Map k v
hit k m@(Map first last limit content) =
    let Just firstK   = first
        Just lastK    = last
        Just lastLnk  = M.lookup lastK content
        adjFront      = M.adjust (\v -> v { lPrev = Just k }) firstK .
                        M.adjust (\v -> v { lPrev = Nothing
                                          , lNext = first
                                          }
                                 ) k
        Just prevLast = lPrev lastLnk
        Just kL       = M.lookup k content
        Just prevK    = lPrev kL
        Just nextK    = lNext kL
    in  case () of _ | k == firstK -> m -- Already at the front
                     | k == lastK  -> -- Move up last
                                      Map (Just k)
                                          (lPrev lastLnk)
                                          limit
                                          -- Second last now last, having no next
                                          . M.adjust (\v -> v { lNext = Nothing })
                                                     prevLast
                                                     . adjFront $ content -- Update the new first
                     | otherwise   -> -- Move to front from the middle
                                      Map (Just k)
                                          last
                                          limit
                                          -- Remove key from the middle
                                          . M.adjust (\v -> v { lNext = Just nextK }) prevK
                                          . M.adjust (\v -> v { lPrev = Just prevK }) nextK
                                          . adjFront $ content -- Update the new first

deleteInternal :: Ord k
               => k                  -- Key to be deleted (must be in the map)
               -> Map k v            -- Map to be deleted from
               -> M.Map k (Link k v) -- Internal M.Map with the key already removed
                                     -- (but the linked list still needs fixing)
               -> Link k v           -- Value of key to be removed
               -> Map k v
deleteInternal k (Map first last limit content) content' kL =
    let Just firstK = first
        Just lastK  = last
        Just nextK  = lNext kL
        Just prevK  = lPrev kL
    in  case () of _ | M.size content == 1 -> -- Just drop the last item
                                              Map Nothing Nothing limit content'
                     | k == firstK         -> -- Remove first
                                              Map (lNext kL)
                                                  last
                                                  limit
                                                  . M.adjust (\v -> v { lPrev = Nothing })
                                                             nextK
                                                             $ content'
                     | k == lastK          -> -- Remove last
                                              Map first
                                                  (lPrev kL)
                                                  limit
                                                  . M.adjust (\v -> v { lNext = Nothing })
                                                             prevK
                                                             $ content'
                     | otherwise           -> -- Remove from the middle, first / last unchanged
                                              Map first
                                                  last
                                                  limit
                                                  . M.adjust (\v -> v { lNext = lNext kL }) prevK
                                                  . M.adjust (\v -> v { lPrev = lPrev kL }) nextK
                                                  $ content'

delete :: Ord k => k -> Map k v -> (Map k v, Maybe v)
delete k m =
    let (mKL, content') = M.updateLookupWithKey (\_ _ -> Nothing) k $ mMap m
    in  maybe (m, Nothing) (\l -> (deleteInternal k m content' l, Just $ lVal l)) mKL

-- Delete and return most recently used item
pop :: Ord k => Map k v -> (Map k v, Maybe (k, v))
pop m =
    if   (fst $ size m) == 0
    then (m, Nothing)
    else let (m', Just v) = delete first m
             Just first   = mFirst m
         in  (m', Just (first, v))

update :: Ord k => k -> v -> Map k v -> Map k v
update k v m =
    case insertInternal True k v m of
        (m', Nothing) -> m'
        _             -> error "LRUBoundedMap.update: insertInternal truncated with updateOnly"

-- Insert a new element into the map, return the new map and the truncated
-- element (if over the limit)
insert ::  Ord k => k -> v -> Map k v -> (Map k v, Maybe (k, v))
insert = insertInternal False

insertInternal :: Ord k => Bool -> k -> v -> Map k v -> (Map k v, Maybe (k, v))
insertInternal updateOnly k v m@(Map first last limit content) =
    let insertEmpty    = Map (Just k)
                             (Just k)
                             limit
                             (M.insert k (Link Nothing Nothing v) content)
        insertUpdate   = ( hit k $ m { mMap = M.adjust (\v' -> v' { lVal = v }) k content }
                         , Nothing
                         )
        insertAdd      = if   M.size content == limit
                         then addFull
                         else (add, Nothing)
        -- Add to the front
        inserted       = M.insert k firstL
                             . M.adjust (\v' -> v' { lPrev = Just k })
                                        firstK
                                        $ content
        add            = m { mFirst = Just k
                           , mMap   = inserted
                           }
        Just firstK    = first
        firstL         = Link Nothing (Just firstK) v
        -- Delete last
        addFull        = (deleteInternal lastK add deleted lastL, Just (lastK, lVal lastL))
        deleted        = M.delete lastK inserted
        Just lastK     = last
        Just lastL     = M.lookup lastK inserted
    -- We can have an empty or a non-empty list, the item can be already in the
    -- map or not, and we can be in insert or update mode, handle all cases below
    in  case () of _ | M.null content && (not updateOnly) -> (insertEmpty, Nothing)
                     | M.member k content                 -> insertUpdate
                     | not updateOnly                     -> insertAdd
                     | otherwise                          -> (m, Nothing)

valid :: Ord k => Map k v -> Maybe String
valid (Map first last limit content) =
    let w = execWriter $ do
                when (limit < 1)              $ tell "limit < 1\n"
                when (not $ M.valid content)  $ tell "Data.Map.Strict not valid\n"
                when (M.size content > limit) $ tell "Size over the limit\n"
                when (length keysForwards /= M.size content) $
                    tell "Map / linked-list size mismatch\n"
                when (keysForwards /= reverse keysBackwards) $
                    tell "Forwards and backwards traversal gives different lists\n"
                when (not $ all (`M.member` content) keysForwards) $
                    tell "Not all keys from the linked-list present in the map\n"
        keysForwards           = traverse (lNext) first
        keysBackwards          = traverse (lPrev) last
        traverse _ Nothing     = []
        traverse step (Just k) = let Just nextK = M.lookup k content
                                 in  k : (traverse (step) $ step nextK)
    in  case w of [] -> Nothing
                  xs -> Just xs

