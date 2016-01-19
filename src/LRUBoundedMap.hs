
{-# LANGUAGE BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}

module LRUBoundedMap ( Map
                     , empty
                     , toList
                     , null
                     , size
                     , member
                     , notMember
                     , insert
                     , update
                     , delete
                     , lookup
                     , lookupNoLRU
                     , popOldest
                     , popNewest
                     , compactTicks
                     , valid
                     ) where

import Prelude hiding (lookup, null)
import qualified Data.Hashable as H
import Data.Hashable (Hashable)
import Data.Bits
import Data.Maybe
import Data.Word
import Data.List hiding (lookup, delete, null, insert)
import Control.Monad
import Control.Monad.Writer
import Control.DeepSeq (NFData(rnf))

-- Associative array implemented on top of a Hashed Trie. Basically a prefix tree over
-- the bits of key hashes. Additional least / most recently used bounds for subtrees are
-- stored so the data structure can have an upper bound on the number of elements and
-- remove the least recently used one overflow. The other bound allows to retrieve the
-- item which was inserted / touched last. Unlike a Data.HashMap our size operation is
-- O(1), we also need to rebuild the map every 2^32 changes (see compactTicks)

type Tick = Word32

data Map k v = Map { mLimit :: !Int
                   , mTick  :: !Tick -- We use a 'tick', which we keep incrementing, to keep
                                     -- track of how old elements are relative to each other
                   , mSize  :: !Int -- Cached to make size O(1) instead of O(n)
                   , mTrie  :: !(Trie k v)
                   }

instance (NFData k, NFData v) => NFData (Map k v) where
    rnf (Map l t s h) = rnf l `seq` rnf t `seq` rnf s `seq` rnf h

type Hash = Int

{-# INLINE hash #-}
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L !k !v !Tick

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L k v _) = rnf k `seq` rnf v

data Trie k v = Empty
                -- Storing the tick interval for both branches instead of an aggregate
                -- makes for slightly faster code at the expense of some storage
                --
                --   Oldest A   Newest A   Oldest B  Newest B    A           B
              | Node !Tick      !Tick      !Tick     !Tick       !(Trie k v) !(Trie k v)
              | Leaf !Hash !(Leaf k v)
              | Collision !Hash ![Leaf k v]

{-# INLINE minMaxFromTrie #-}
minMaxFromTrie :: Trie k v -> (Tick, Tick)
minMaxFromTrie Empty                          = (maxBound, minBound)
minMaxFromTrie (Node olda newa oldb newb _ _) = (min olda oldb, max newa newb)
minMaxFromTrie (Leaf _ (L _ _ tick))          = (tick, tick)
minMaxFromTrie (Collision _ ch)               = ( minimum . map (\(L _ _ tick) -> tick) $ ch
                                                , maximum . map (\(L _ _ tick) -> tick) $ ch
                                                )

instance (NFData k, NFData v) => NFData (Trie k v) where
    rnf Empty              = ()
    rnf (Leaf _ l)         = rnf l
    rnf (Node _ _ _ _ a b) = rnf a `seq` rnf b
    rnf (Collision _ ch)   = rnf ch

empty :: Int -> Map k v
empty limit | limit < 1                    = error   "limit for LRUBoundedMap needs to be >= 1"
            | limit > fromIntegral maxTick = error $ "limit for LRUBoundedMap needs to be <= "
                                                     ++ show maxTick 
            | otherwise = Map { mLimit = limit
                              , mTick  = minBound
                              , mSize  = 0
                              , mTrie  = Empty
                              }
            where -- Probably doesn't make much sense to go higher than this
                  maxTick = (maxBound :: Tick) `div` 2

{-# INLINE isA #-}
{-# INLINE isB #-}
isA, isB :: Hash -> Int -> Bool
isA h s = h .&. (1 `unsafeShiftL` s) == 0
isB h s = not $ isA h s

-- Are two hashes colliding at the given depth?
{-# INLINE subkeyCollision #-}
subkeyCollision :: Hash -> Hash -> Int -> Bool
subkeyCollision a b s = (a `xor` b) .&. (1 `unsafeShiftL` s) == 0

-- As we keep incrementing the global tick, we eventually run into the situation where it
-- overflows (2^32). We could switch to a 64 bit tick, but GHC's emulation for 64 bit
-- operations on 32 bit architectures is very slow. One way around this would be to abuse
-- a Double as a 53 bit integer like this:
--
--
-- newtype Tick53 = Tick53 { tickDouble :: Double }
--                  deriving (Eq, Ord)
--
-- nextTick :: Tick53 -> Tick53
-- nextTick (Tick53 t) = Tick53 (t + 1)
--
-- instance NFData Tick53 where
--     rnf _ = ()
--
-- instance Bounded Tick53 where
--     maxBound = Tick53 9007199254740992
--     minBound = Tick53 0
--
--
-- This is reasonably fast on 32 bit GHC, but still causes a lot of overhead for storing
-- 64 bit values all over the tree. The solution chosen here is to use a 32 bit (or less)
-- tick and simply compact all the ticks in the map when we overflow. This happens very
-- rarely, so it's a clear win for time and space complexity
--
compactTicks :: (Eq k, Hashable k) => Map k v -> Map k v
compactTicks m = go m . empty $ mLimit m
    where go msrc mdst = let (msrc', mkv) = popOldest msrc
                         in  case mkv of Just (k, v) -> go msrc' . fst $ insert k v mdst
                                         Nothing     -> mdst

{-# INLINE compactIfAtTickLimit #-}
compactIfAtTickLimit :: (Eq k, Hashable k) => Map k v -> Map k v
compactIfAtTickLimit m = if   mTick m == maxBound
                         then compactTicks m
                         else m

{-# INLINE popOldestIfAtSizeLimit #-}
popOldestIfAtSizeLimit :: (Eq k, Hashable k) => Map k v -> (Map k v, Maybe (k, v))
popOldestIfAtSizeLimit m = if   mSize m > mLimit m
                           then popOldest m
                           else (m, Nothing)

-- Insert a new element into the map, return the new map and the truncated
-- element (if over the limit)
{-# INLINEABLE insert #-}
insert :: (Eq k, Hashable k) => k -> v -> Map k v -> (Map k v, Maybe (k, v))
insert kIns vIns m =
    let go h k v s (Node mina maxa minb maxb a b) =
            let !(!(!tA, !(!mintA, !maxtA), !insA), !(!tB, !(!mintB, !maxtB), !insB)) =
                    -- Traverse into child with matching subkey
                    if   isA h s
                    then (go h k v (s + 1) a      , (b, (minb, maxb), False))
                    else ((a, (mina, maxa), False), go h k v (s + 1) b      )
                !mint = min mintA mintB
                !maxt = max maxtA maxtB
            in  ( Node mintA maxtA mintB maxtB tA tB
                , (mint, maxt)
                , insA || insB
                )
        go h k v _ Empty = (Leaf h $ L k v tick, (tick, tick), True)
        go h k v s t@(Leaf lh li@(L lk _ lt)) =
            let !mint = min tick lt
                !maxt = max tick lt
            in  if   h == lh
                then if   k == lk
                     then (Leaf h $ L k v tick, (tick, tick), False) -- Update value
                     else -- We have a hash collision, change to a collision node and insert
                          (Collision h [L k v tick, li], (mint, maxt), True)
                else -- Expand leaf into interior node
                     let !(!(!a', !mina, !maxa), !(!b', !minb, !maxb))
                             | subkeyCollision h lh s =
                                   -- Subkey collision, add one level
                                   (if   isA h s
                                    then flip (,) (Empty, maxBound, minBound)
                                    else      (,) (Empty, maxBound, minBound))
                                        . (\(x, (minx, maxx), True) -> (x, minx, maxx))
                                        $ go h k v (s + 1) t
                             | otherwise =
                                   let t' = Leaf h (L k v tick)
                                   in  ( if isA h  s then (t', tick, tick) else (t , lt  , lt  )
                                       , if isB lh s then (t , lt  , lt  ) else (t', tick, tick)
                                       )
                     in ( Node mina maxa minb maxb a' b'
                        , (mint, maxt)
                        , True
                        )
        go h k v s t@(Collision colh ch) =
            if   h == colh
            then let trav [] = [L k v tick] -- Append new leaf
                     trav (l@(L lk _ _):xs) =
                         if   lk == k
                         then L k v tick : xs -- Update value
                         else l : trav xs
                     t' = Collision h $ trav ch
                 in (t', minMaxFromTrie t', length ch /= length (trav ch))
            else -- Expand collision into interior node
                 let (mint, maxt) = minMaxFromTrie t
                 in  go h k v s $
                         if   isA colh s
                         then Node mint     maxt     maxBound minBound t     Empty
                         else Node maxBound minBound mint     maxt     Empty t
        !(trie', _, !didInsert) = go (hash kIns) kIns vIns 0 $ mTrie m
        !tick                   = mTick m
        !inserted               = m { mTrie = trie'
                                    , mSize = mSize m + if didInsert then 1 else 0
                                    , mTick = tick + 1
                                    }
    in  popOldestIfAtSizeLimit . compactIfAtTickLimit $ inserted

-- TODO: This function is just a wrapper around insert, could write optimized version
{-# INLINEABLE update #-}
update :: (Eq k, Hashable k) => k -> v -> Map k v -> Map k v
update k v m | member k m = case insert k v m of
                                (m', Nothing) -> m'
                                _             -> error $ "LRUBoundedMap.update: insert" ++
                                                         " truncated during update"
             | otherwise  = m

{-# INLINEABLE size #-}
size :: Map k v -> (Int, Int)
size m = (mSize m, mLimit m)

-- O(n) size-by-traversal
sizeTraverse :: Map k v -> Int
sizeTraverse m = go $ mTrie m
    where go Empty              = 0
          go (Leaf _ _)         = 1
          go (Node _ _ _ _ a b) = go a + go b
          go (Collision _ ch)   = length ch

{-# INLINEABLE null #-}
null :: Map k v -> Bool
null m = case mTrie m of Empty -> True; _ -> False

{-# INLINEABLE member #-}
member :: (Eq k, Hashable k) => k -> Map k v -> Bool
member k m = isJust $ lookupNoLRU k m

{-# INLINEABLE notMember #-}
notMember :: (Eq k, Hashable k) => k -> Map k v -> Bool
notMember k m = not $ member k m

{-# INLINEABLE toList #-}
toList :: Map k v -> [(k, v)]
toList m = go [] $ mTrie m
    where go l Empty              = l
          go l (Leaf _ (L k v _)) = (k, v) : l
          go l (Node _ _ _ _ a b) = go (go l a) b
          go l (Collision _ ch)   = foldr (\(L k v _) l' -> (k, v) : l') l ch

-- Lookup element, also update LRU
{-# INLINEABLE lookup #-}
lookup :: (Eq k, Hashable k) => k -> Map k v -> (Map k v, Maybe v)
lookup k' m = if   isNothing mvalue -- Only increment tick if we found something
              then (m, Nothing)
              else (compactIfAtTickLimit $ m { mTick = mTick m + 1, mTrie = trie' }, mvalue)
    where go :: Eq k => Hash -> k -> Tick -> Int -> Trie k v -> (Maybe v, Trie k v)
          go h k tick s t@(Node mina maxa minb maxb a b) =
              -- Traverse into child with matching subkey
              let !(!ins, t') = go h k tick (s + 1) (if isA h s then a else b)
              in  if   isNothing ins -- Don't rebuild node if we don't need to update the LRU
                  then (Nothing, t)
                  else let !(!mint', !maxt') = minMaxFromTrie t'
                       in  if   isA h s
                           then (ins, Node mint' maxt' minb  maxb  t' b )
                           else (ins, Node mina  maxa  mint' maxt' a  t')
          go !_ !_ _ !_ Empty = (Nothing, Empty)
          go h k tick _ t@(Leaf lh (L lk lv _))
              | lh /= h   = (Nothing, t)
              | lk /= k   = (Nothing, t)
              | otherwise = (Just lv, Leaf lh (L lk lv tick))
          go h k tick _ t@(Collision colh ch)
              | colh == h = -- Search child list for matching key, rebuild with updated tick
                            foldl' (\(r, Collision _ ch') l@(L lk lv _) ->
                                       if   lk == k
                                       then (Just lv, Collision colh $ L lk lv tick : ch')
                                       else (r      , Collision colh $ l            : ch')
                                   )
                                   (Nothing, Collision colh [])
                                   ch
              | otherwise = (Nothing, t)
          !(!mvalue, !trie') = go (hash k') k' (mTick m) 0 $ mTrie m

{-# INLINEABLE lookupNoLRU #-}
lookupNoLRU :: (Eq k, Hashable k) => k -> Map k v -> Maybe v
lookupNoLRU k' m = go (hash k') k' 0 $ mTrie m
    where go h k s (Node _ _ _ _ a b) = go h k (s + 1) (if isA h s then a else b)
          go !_ !_ !_ Empty = Nothing
          go h k _ (Leaf lh (L lk lv _))
              | lh /= h   = Nothing
              | lk /= k   = Nothing
              | otherwise = Just lv
          go h k _ (Collision colh ch)
              | colh == h = (\(L _ lv _) -> lv) <$> find (\(L lk _ _) -> lk == k) ch
              | otherwise = Nothing

{-# INLINEABLE delete #-}
delete :: (Eq k, Hashable k) => k -> Map k v -> (Map k v, Maybe v)
delete k' m =
    let go h k s t@(Node _ _ _ _ a b) =
            let !(ch, del') = if   isA h s
                              then (\(!t', !dchild) -> ((t', b ), dchild)) $ go h k (s + 1) a
                              else (\(!t', !dchild) -> ((a , t'), dchild)) $ go h k (s + 1) b
            in  if   isNothing del'
                then (t, Nothing)
                else ( case ch of
                           -- We removed the last element, delete node
                           (Empty, Empty)                        -> Empty
                           -- If our last child is a leaf / collision replace the node by it
                           (Empty, t'   ) | isLeafOrCollision t' -> t'
                           (t'   , Empty) | isLeafOrCollision t' -> t'
                           -- Update node with new subtree
                           !(!a', !b')                           ->
                               -- TODO: Don't recompute min/max for static branch
                               let (minA, maxA) = minMaxFromTrie a'
                                   (minB, maxB) = minMaxFromTrie b'
                               in  Node minA maxA minB maxB a' b'
                     , del'
                     )
        go !_ !_ !_ Empty = (Empty, Nothing)
        go h k _ t@(Leaf lh (L lk lv _))
            | lh /= h   = (t, Nothing)
            | lk /= k   = (t, Nothing)
            | otherwise = (Empty, Just lv)
        go h k _ t@(Collision colh ch)
            | colh == h = let (delch', ch') = partition (\(L lk _ _) -> lk == k) ch
                          in  if   length ch' == 1
                              then  -- Deleted last remaining collision, it's a leaf node now
                                   (Leaf h $ head ch', Just $ (\((L _ lv _) : []) -> lv) delch')
                              else (Collision h ch', (\(L _ lv _) -> lv) <$> listToMaybe delch')
            | otherwise = (t, Nothing)
        !(m', del) = go (hash k') k' 0 $ mTrie m
        isLeafOrCollision (Leaf _ _)      = True
        isLeafOrCollision (Collision _ _) = True
        isLeafOrCollision _               = False
    in  if   isNothing del
        then (m, Nothing)
        else ( m { mTrie = m'
                 , mSize = mSize m - 1
                 }
             , del
             )

popNewest, popOldest :: (Eq k, Hashable k) => Map k v -> (Map k v, Maybe (k, v))
popNewest = popInternal False
popOldest = popInternal True

-- Delete and return most / least recently used item
--
-- TODO: We first find the item and then delete it by key, could do this with a
--       single traversal instead
popInternal :: (Eq k, Hashable k) => Bool -> Map k v -> (Map k v, Maybe (k, v))
popInternal popOld m =
    case go $ mTrie m of
        Just k  -> let !(!m', !(Just v)) = delete k m in (m', Just (k, v))
        Nothing -> (m, Nothing)
    where go Empty                          = Nothing
          go (Leaf _ (L lk _ _))            = Just lk
          go (Node mina maxa minb maxb a b) = go $ if   popOld
                                                   then if mina < minb then a else b
                                                   else if maxa > maxb then a else b
          go (Collision _ ch)               = Just . (\(L lk _ _) -> lk)
                                                   . ( if   popOld
                                                       then minimumBy
                                                       else maximumBy
                                                     )
                                                     (\(L _ _ a) (L _ _ b) -> compare a b)
                                                     $ ch

-- Run a series of consistency checks on the structure inside of the map, return a list of
-- errors if any issues where encountered
valid :: (Eq k, Hashable k, Eq v) => Map k v -> Maybe String
valid m =
    let w =
         execWriter $ do
             when (mLimit m < 1) $
                 tell "Invalid limit (< 1)\n"
             when (fst (size m) /= sizeTraverse m) $
                 tell "Mismatch beween cached and actual size\n"
             when (fst (size m) > mLimit m) $
                 tell "Size over the limit\n"
             allTicks <-
               let trav s minParent maxParent ticks t =
                     case t of
                         Leaf h (L lk lv lt) ->
                             (: ticks) <$> checkKey h lk lv lt minParent maxParent
                         Collision h ch -> do
                             -- tell "Found collision\n"
                             when (length ch < 2) $
                                 tell "Hash collision node with <2 children\n"
                             foldM (\xs (L lk lv lt) ->
                                       (: xs) <$> checkKey h lk lv lt minParent maxParent
                                   )
                                   ticks
                                   ch
                         Node minA maxA minB maxB a b -> do
                             let mint = min minA minB
                                 maxt = max maxA maxB
                             when (s + 1 > finiteBitSize (undefined :: Word)) $
                                 tell "Subkey shift too large during traversal\n"
                             when (mint < minParent || maxt > maxParent) $
                                 tell "Node min/max tick outside of parent interval\n"
                             let used = foldr (\x@(t', _, _) u ->
                                                  case t' of Empty -> u; _ -> x : u
                                              )
                                        []
                                        $ [(a, minA, maxA), (b, minB, maxB)]
                             when (length used == 0) $
                                 tell "Node with only empty children\n"
                             when (length used == 1) $
                                case (\((x, _, _) : _) -> x) used of
                                    Leaf      _ _ -> tell "Node with single Leaf child\n"
                                    Collision _ _ -> tell "Node with single Collision child\n"
                                    _             -> return ()
                             foldM (\xs (c, mint', maxt') ->
                                       trav (s + 1) mint' maxt' xs c
                                   )
                                   ticks
                                   used
                         Empty -> return ticks
                   checkKey h k v tick minParent maxParent = do
                       when (hash k /= h) $
                           tell "Hash / key mismatch\n"
                       when (tick >= mTick m) $
                           tell "Tick of leaf matches / exceeds current tick\n"
                       when (tick < minParent || tick > maxParent) $
                           tell "Leaf min/max tick outside of parent interval\n"
                       case snd $ lookup k m of
                           Nothing ->
                               tell "Can't lookup key found during traversal\n"
                           Just v' -> when (v /= v') .
                               tell $ "Lookup of key found during traversal yields " ++
                                      "different value\n"
                       let (m', v') = delete k m
                       when (fst (size m') /= (fst $ size m) - 1) $
                           tell "Deleting key did not reduce size\n"
                       when (fromMaybe v v' /= v) $
                           tell "Delete returned wrong value\n"
                       return tick
               in  trav 0 minBound maxBound [] $ mTrie m
             when (length allTicks /= mSize m) $
                 tell "Collection of all tick values used resulted in different size that mSize\n"
             unless (not . any (\x -> length x /= 1) . group . sort $ allTicks) $
                 tell "Duplicate tick value found\n"
             let keysL      = map fst $ toList m
                 allDeleted = foldl' (\r k -> fst $ delete k r) m keysL
             when (length keysL /= fst (size m)) $
                 tell "Length of toList does not match size\n"
             unless (null allDeleted) $
                 tell "Deleting all elements does not result in an empty map\n"
             unless (fst (size allDeleted) == 0) $
                 tell "Deleting all elements does not result in a zero size map\n"
             let compacted = compactTicks m
             when (snd (popOldest m) /= snd (popOldest compacted) ||
                   snd (popNewest m) /= snd (popNewest compacted)) $
                  tell "Tick compaction changes LRU\n"
             when (toList m /= toList compacted) $
                  tell "Tick compaction changes map\n"
             when (fromIntegral (mTick compacted) /= fst (size compacted)) $
                  tell "Tick compaction did not reduce tick range to minimum\n"
    in  case w of [] -> Nothing
                  xs -> Just xs

