{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Char (isDigit)
import Data.Heap qualified as H
import Data.List.Extra (maximumOn)
import Data.List.Split (wordsBy)

type Pos = (Int, Int, Int)

data Bot = Bot {pos :: !Pos, rad :: !Int}

dist :: Pos -> Pos -> Int
dist (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

data Cube = Cube {cx, cy, cz, size :: !Int}

data Key = Key {botsK, distK, sizeK :: !Int} deriving (Eq, Ord)

keyFor :: [Bot] -> Cube -> Key
keyFor bots cube = Key count (originDist cube) (size cube)
  where
    count = -sum [1 | Bot p r <- bots, cubeDist p cube <= r]

cubeDist :: Pos -> Cube -> Int
cubeDist (px, py, pz) (Cube x y z s) = f x px + f y py + f z pz
  where
    f lo p = max 0 (lo - p) + max 0 (p - lo - s + 1)

originDist :: Cube -> Int
originDist = cubeDist (0, 0, 0)

-- $setup
-- >>> input = "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2018/23.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Bot
parse s = Bot (x, y, z) r
  where
    [x, y, z, r] = read <$> wordsBy (\c -> not $ isDigit c || c == '-') s

-- >>> solve example
-- >>> partTwo example
-- 7
-- 1
solve, partTwo :: [Bot] -> Int
solve bots = sum [1 | b <- bots, dist (pos strongest) (pos b) <= rad strongest]
  where
    strongest = maximumOn rad bots
partTwo bots = go $ H.singleton (withScore start)
  where
    maxC = maximum [abs c | Bot (x, y, z) _ <- bots, c <- [x, y, z]]
    base = until (>= maxC) (* 2) 1
    start = Cube (-base) (-base) (-base) (base * 2)
    withScore c = (keyFor bots c, c)

    go :: H.MinPrioHeap Key Cube -> Int
    go (H.view -> Just ((_, cube), h))
      | size cube == 1 = originDist cube
      | otherwise = go $ foldr (H.insert . withScore) h (splitCube cube)

    splitCube (Cube x y z s) =
      [Cube (x + dx) (y + dy) (z + dz) h | dx <- [0, h], dy <- [0, h], dz <- [0, h]]
      where
        h = s `div` 2
