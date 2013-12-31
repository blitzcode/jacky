
module RectPacker ( empty
                  , pack
                  , packRectangles
                  , RectPacker
                  , dimensions
                  ) where

import Data.Maybe
import Data.List

-- KDTree for finding a tight layout for a number of rectangles inside a larger bounding
-- rectangle. Useful for packing textures (fonts, lightmaps), UI elements, etc.

-- TODO: We could do a lot better, see http://clb.demon.fi/files/RectangleBinPack.pdf

-- TODO: Keep track of free space, so we can sort multiple rectangles packers by that criterion

data RectPacker = RectPacker !KDTree !Int !Int deriving (Show)

data KDTree = Split                !Bool
                    {-# UNPACK #-} !Int 
                                   !KDTree
                                   !KDTree
            | Used
            | Empty
              deriving (Show)

dimensions :: RectPacker -> (Int, Int)
dimensions (RectPacker _ w h) = (w, h)

empty :: Int -> Int -> RectPacker
empty w h = RectPacker Empty w h

pack :: Int -> Int -> RectPacker -> (RectPacker, Maybe (Int, Int))
pack insWdh insHgt (RectPacker root rootWdh rootHgt) =
    let go :: Int -> Int -> Int -> Int -> KDTree -> (KDTree, Maybe (Int, Int))
        go x y w h node = 
            if   w < insWdh || h < insHgt -- Early exit when the node is too small
            then (node, Nothing)
            else case node of
                Split vert pos a b ->
                    let (kdA, coordA) | vert     = go x y pos h   a
                                      | not vert = go x y w   pos a
                        (kdB, coordB) | vert     = go (x + pos)  y        (w - pos)  h        b
                                      | not vert = go  x        (y + pos)  w        (h - pos) b
                    in  case () of
                                -- First, try to insert left (if we create a new split, we
                                -- trim the left side to perfect size, hence we try it first)
                            _ | isJust coordA -> (Split vert pos kdA b, coordA)
                                -- 
                                -- For the right side, we have a special optimization. When
                                -- inserting many equally sized rectangles, our tree can
                                -- degenerate from O(log n) to O(n). To fix this, we merge
                                -- all those rectangles into a single one by moving the left
                                -- side of child split into the adjacent parent's left side
                                -- (just move the split position)
                                --
                                -- Example:
                                --
                                -- OOOOOOOOOOOOOOOOOOOOOO
                                -- O                    O
                                -- OOOOOOOOOOOOOOOOOOOOOO
                                -- O....O....O....O     O
                                -- O....O....O....O     O
                                -- OOOOOOOOOOOOOOOOOOOOOO
                                --
                                -- In this tree we inserted just three rectangles, but
                                -- already need four traversal steps for the next
                                -- insertion. With this optimization, all the bottom
                                -- rectangles get merged into a single space
                                --
                              | isJust coordB -> let def = (Split vert pos a kdB, coordB) in
                                  case kdB of (Split vert' pos' Used b') ->
                                                  if   vert == vert' -- Same axis?
                                                  then (Split vert (pos + pos') a b', coordB)
                                                  else def
                                              _                          -> def
                              | otherwise     -> (node, Nothing)
                Used  -> (Used, Nothing)
                Empty ->
                    case () of
                        _ | w == insWdh && h == insHgt -> (Used, Just (x, y)) -- Perfect fit
                          | w - insWdh > h - insHgt    -> go x y w h          -- Vertical split
                                                              (Split True  insWdh Empty Empty)
                          | otherwise                  -> go x y w h          -- Horizontal split
                                                              (Split False insHgt Empty Empty)
        (newRoot, maybePos) = go 0 0 rootWdh rootHgt root
    in  (RectPacker newRoot rootWdh rootHgt, maybePos)

packRectangles :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int, Int, Int)]
packRectangles rootWdh rootHgt padding sizes =
    fst $ foldl'
        (\(xs, rp) (w, h) -> case pack (w + padding) (h + padding) rp of
                                 (rp', Just (x, y)) -> ((x, y, w, h) : xs, rp')
                                 (_  , Nothing    ) -> (xs, rp)
        )
        ([], empty rootWdh rootHgt)
        sizes

