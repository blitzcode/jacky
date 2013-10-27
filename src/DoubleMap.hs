
module DoubleMap ( Map
                 , empty
                 , insert
                 , member
                 , notMember
                 , lookup
                 , delete
                 , deleteFindMaxA
                 , deleteFindMaxB
                 , view
                 , update
                 , updateKeyA
                 , updateKeyB
                 , size
                 , null
                 ) where

import qualified Data.Map.Strict as M
import Prelude hiding (lookup, null)
import Control.Applicative hiding (empty)

-- Map sorted and indexed by two different types of key (assumes both keys are
-- unique for each element)

data Map ka kb v = Map !(M.Map ka (kb, v))
                       !(M.Map kb (ka, v))
                       deriving (Show)

empty :: Map ka kb v
empty = Map M.empty M.empty

member :: (Ord ka, Ord kb) => Either ka kb -> Map ka kb v -> Bool
member k (Map ma mb) = case k of Left  ka -> M.member ka ma
                                 Right kb -> M.member kb mb

notMember :: (Ord ka, Ord kb) => Either ka kb -> Map ka kb v -> Bool
notMember k m = not $ member k m

-- TODO: This will leak when we overwrite existing entries with different A/B keys
insert :: (Ord ka, Ord kb) => ka -> kb -> v -> Map ka kb v -> Map ka kb v
insert ka kb v (Map ma mb) = v `seq` Map (M.insert ka (kb, v) ma)
                                         (M.insert kb (ka, v) mb)

lookup :: (Ord ka, Ord kb) => Either ka kb -> Map ka kb v -> Maybe (ka, kb, v)
lookup k (Map ma mb) = case k of Left  ka -> (\(kb, v) -> (ka, kb, v)) <$> M.lookup ka ma
                                 Right kb -> (\(ka, v) -> (ka, kb, v)) <$> M.lookup kb mb

delete :: (Ord ka, Ord kb) => Either ka kb -> Map ka kb v -> Map ka kb v
delete k m@(Map ma mb) = case lookup k m of
    Just (ka, kb, _) -> Map (M.delete ka ma) (M.delete kb mb)
    Nothing          -> m

-- Find the largest key of A/B, delete it from the map and return it
deleteFindMaxA :: (Ord ka, Ord kb) => Map ka kb v -> (Map ka kb v, Maybe (ka, kb, v))
deleteFindMaxA m@(Map ma mb) = if   null m
                               then (m, Nothing)
                               else let ((delKeyA, (delKeyB, delVal)), delMapA) =
                                            M.deleteFindMax ma
                                    in  ( Map delMapA (M.delete delKeyB mb)
                                        , Just (delKeyA, delKeyB, delVal)
                                        )
deleteFindMaxB :: (Ord ka, Ord kb) => Map ka kb v -> (Map ka kb v, Maybe (ka, kb, v))
deleteFindMaxB m@(Map ma mb) = if   null m
                               then (m, Nothing)
                               else let ((delKeyB, (delKeyA, delVal)), delMapB) =
                                            M.deleteFindMax mb
                                    in  ( Map (M.delete delKeyA ma) delMapB
                                        , Just (delKeyA, delKeyB, delVal)
                                        )

view :: Map ka kb v -> (M.Map ka (kb, v), M.Map kb (ka, v))
view (Map ma mb) = (ma, mb)

update :: (Ord ka, Ord kb) => Either ka kb -> v -> Map ka kb v -> Map ka kb v
update k v m@(Map ma mb) = case lookup k m of
    Just (ka, kb, _) -> Map (M.update (\_ -> Just (kb, v)) ka ma)
                            (M.update (\_ -> Just (ka, v)) kb mb)
    Nothing          -> m

updateKeyA :: (Ord ka, Ord kb) => ka -> ka -> Map ka kb v -> Map ka kb v
updateKeyA ka ka' m@(Map ma mb) = case lookup (Left ka) m of
    Just (_, kb, v) -> Map (M.insert ka' (kb, v) $ M.delete ka ma)
                           (M.update (\_ -> Just (ka', v)) kb mb)
    Nothing         -> m

updateKeyB :: (Ord ka, Ord kb) => kb -> kb -> Map ka kb v -> Map ka kb v
updateKeyB kb kb' m@(Map ma mb) = case lookup (Right kb) m of
    Just (ka, _, v) -> Map (M.update (\_ -> Just (kb', v)) ka ma)
                           (M.insert kb' (ka, v) $ M.delete kb mb)
    Nothing         -> m

size :: Map ka kb v -> Int
size (Map ma _) = M.size ma

null :: Map ka kb v -> Bool
null (Map ma _) = M.null ma

-- TODO: Add verify function

