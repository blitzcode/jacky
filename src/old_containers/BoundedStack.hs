
module BoundedStack ( BoundedStack
                    , mkBoundedStack
                    , push
                    , push_
                    , pop
                    ) where

import qualified Data.Sequence as S

-- Stack implementation based on Sequence which drops elements pushed over
-- a specified depth

data BoundedStack a = BoundedStack (S.Seq a) !Int
                      deriving (Show)

mkBoundedStack :: Int -> BoundedStack a
mkBoundedStack limit | limit >= 1 = BoundedStack S.empty limit
                     | otherwise  = error "limit for BoundedStack needs to be >= 1"

-- Push element on the stack, truncate at the other end if we reached the limit,
-- return new stack and truncated element (if over the limit)
push :: a -> BoundedStack a -> (BoundedStack a, Maybe a)
push x (BoundedStack s limit) =
    let seqDropR sd = case S.viewr sd of (s' S.:> e) -> (s', Just e)
                                         S.EmptyR    -> (sd, Nothing)
        boundedS | S.length s >= limit = seqDropR s
                 | otherwise           = (s, Nothing)
    in  case boundedS of (s', e) -> (BoundedStack (x S.<| s') limit, e)

push_ :: a -> BoundedStack a -> BoundedStack a
push_ x s = fst $ push x s

-- LIFO pop
pop :: BoundedStack a -> (Maybe a, BoundedStack a)
pop bs@(BoundedStack s limit) =
    case S.viewl s of (x S.:< s') -> (Just x , BoundedStack s' limit)
                      S.EmptyL    -> (Nothing, bs)

