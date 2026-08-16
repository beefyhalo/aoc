{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Array (Array, bounds, inRange, listArray, range, (!))
import Data.Char (isDigit)
import Data.Heap qualified as H
import Data.List.Split (wordsBy)
import Data.Map.Strict qualified as M

data Tool = Neither | Torch | Gear deriving (Eq, Ord, Enum, Bounded, Show)

type Pos = (Int, Int)

type State = (Int, Pos, Tool)

allowed :: Int -> [Tool]
allowed reg = [t | t <- [minBound ..], fromEnum t /= reg]

region :: Array Pos Int -> Pos -> Int
region g p = g ! p `mod` 3

main :: IO ()
main = print . solve . parse =<< readFile "input/2018/22.txt"

parse :: String -> (Int, Pos)
parse s = (d, (x, y))
  where
    [d, x, y] = read <$> wordsBy (not . isDigit) s

-- >>> solve (510, (10, 10))
-- (114,45)
solve :: (Int, Pos) -> (Int, Int)
solve (depth, target) = (risk, time)
  where
    grid = erosion target depth
    risk = sum $ region grid <$> range ((0, 0), target)
    time = dijkstra grid target

erosion :: Pos -> Int -> Array Pos Int
erosion (tx, ty) depth = arr
  where
    bnds = ((0, 0), (tx + 10, ty + 10))
    arr = listArray bnds $ level <$> range bnds

    level p@(x, y)
      | p == (0, 0) || p == (tx, ty) = depth `mod` 20183
      | y == 0 = (x * 16807 + depth) `mod` 20183
      | x == 0 = (y * 48271 + depth) `mod` 20183
      | otherwise = (arr ! (x - 1, y) * arr ! (x, y - 1) + depth) `mod` 20183

dijkstra :: Array Pos Int -> Pos -> Int
dijkstra grid target = go (H.singleton (0, (0, 0), Torch)) M.empty
  where
    go :: H.MinHeap State -> M.Map (Pos, Tool) Int -> Int
    go (H.view -> Just ((c, p, t), heap)) dist
      | p == target && t == Torch = c
      | Just d <- M.lookup (p, t) dist, d <= c = go heap dist
      | otherwise = go heap' dist'
      where
        dist' = M.insert (p, t) c dist
        heap' = foldr H.insert heap (neighbors grid (c, p, t))

    neighbors :: Array Pos Int -> State -> [State]
    neighbors g (cost, p@(x, y), tool) = tools ++ moves
      where
        tools = [(cost + 7, p, t) | t <- allowed (region g p), t /= tool]
        moves =
          [ (cost + 1, p', tool)
          | p' <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)],
            bounds g `inRange` p',
            tool `elem` allowed (region g p')
          ]