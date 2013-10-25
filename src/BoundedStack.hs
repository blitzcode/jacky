
module BoundedStack ( BoundedStack
                    , mkBoundedStack
                    , pushBoundedStack
                    , pushBoundedStack_
                    , popBoundedStack
                    ) where

import qualified Data.Sequence as S

-- Stack implementation based on Sequence which drops elements pushed over
-- a specified depth

data BoundedStack a = BoundedStack (S.Seq a) Int
                      deriving (Show)

mkBoundedStack :: Int -> BoundedStack a
mkBoundedStack limit | limit >= 1 = BoundedStack S.empty limit
                     | otherwise  = error "limit for BoundedStack needs to be >= 1"

-- Push element on the stack, truncate at the other end if we reached the limit,
-- return new stack and truncated element (if over the limit)
pushBoundedStack :: a -> BoundedStack a -> (BoundedStack a, Maybe a)
pushBoundedStack x (BoundedStack s limit) =
    let seqDropR sd = case S.viewr sd of (s' S.:> e) -> (s', Just e)
                                         S.EmptyR    -> (sd, Nothing)
        boundedS | S.length s >= limit = seqDropR s
                 | otherwise           = (s, Nothing)
    in  case boundedS of (s', e) -> (BoundedStack (x S.<| s') limit, e)

pushBoundedStack_ :: a -> BoundedStack a -> BoundedStack a
pushBoundedStack_ x s = fst $ pushBoundedStack x s

-- LIFO pop
popBoundedStack :: BoundedStack a -> (Maybe a, BoundedStack a)
popBoundedStack bs@(BoundedStack s limit) =
    case S.viewl s of (x S.:< s') -> (Just x , BoundedStack s' limit)
                      S.EmptyL    -> (Nothing, bs)

