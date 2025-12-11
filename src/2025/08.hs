{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Data.Function (on)
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import qualified Data.IntMap.Monoidal.Strict as M
import qualified Data.IntSet as S
import Data.List (foldl', sortOn, unfoldr)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down))
import GHC.Arr (listArray, (!))
import Linear.V3 (V3 (..))

-- $setup
-- >>> input = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2025/08.txt"
  print $ solve 1000 input
  print $ partTwo input

parse :: String -> V3 Int
parse s = case splitOn "," s of
  [a, b, c] -> V3 (read a) (read b) (read c)

-- >>> solve 10 example
-- 40
solve :: Int -> [V3 Int] -> Int
solve q points = product $ take 3 $ sortOn Down sizes
  where
    n = length points
    pairs = take q $ sortedPairs points
    start = M.fromAscList [(i, i) | i <- [1 .. n]]
    final = foldl' (\uf (a, b) -> union uf a b) start pairs
    sizes = componentSizes final

sortedPairs :: [V3 Int] -> [(Int, Int)]
sortedPairs points = sortOn dist2 [(a, b) | a <- [1 .. n - 1], b <- [a + 1 .. n]]
  where
    n = length points
    ary = listArray (1, n) points
    dist2 (a, b) = sum $ (ary ! a - ary ! b) ^ 2

type UnionFind = MonoidalIntMap Int

find :: UnionFind -> Int -> Int
find parent x = case M.lookup x parent of
  Just p | p /= x -> find parent p
  _ -> x

union :: UnionFind -> Int -> Int -> UnionFind
union parent a b
  | ra == rb = parent
  | otherwise = M.insert ra rb parent
  where
    ra = find parent a
    rb = find parent b

componentSizes :: UnionFind -> [Int]
componentSizes parent = map S.size $ M.elems components
  where
    components = M.foldMapWithKey (\k _ -> M.singleton (find parent k) (S.singleton k)) parent

-- >>> partTwo example
-- 25272
partTwo :: [V3 Int] -> Int
partTwo points = ans
  where
    n = length points
    ary = listArray (1, n) points
    pairs = sortedPairs points
    start = (M.fromAscList [(i, i) | i <- [1 .. n]], n, pairs)

    -- pick next edge that merges components, stop when comps == 1
    go (_, 1, _) = Nothing
    go (uf, comps, ps) = case dropWhile (uncurry ((==) `on` find uf)) ps of
      (x, y) : rest -> Just ((x, y), (union uf x y, comps - 1, rest))

    -- build MST edges, stop when fully connected
    (a, b) = last $ unfoldr go start
    V3 ans _ _ = ary ! a * ary ! b
