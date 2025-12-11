{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Either (partitionEithers)
import Data.List (sortOn)

type Coord = (Int, Int)

type HLine = (Int, Int, Int) -- y, x_min, x_max

type VLine = (Int, Int, Int) -- x, y_min, y_max

-- $setup
-- >>> input = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2025/09.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Coord
parse s = case break (== ',') s of
  (a, ',' : b) -> (read a, read b)

solve :: [Coord] -> Int
solve poly = maximum [area a b | a <- poly, b <- poly]

area :: Coord -> Coord -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

-- >>> partTwo example
-- 6
partTwo :: [Coord] -> Int
partTwo poly =
  maximum
    [ area a b | a@(ax, ay) <- poly, b@(bx, by) <- poly, checkRect (min ax bx, min ay by, max ax bx, max ay by)
    ]
  where
    (h, v) = edges
    hSorted = sortOn (\(y, _, _) -> y) h
    vSorted = sortOn (\(x, _, _) -> x) v

    -- pair up consecutive polygon vertices into horizontal/vertical segments
    edges :: ([HLine], [VLine])
    edges = partitionEithers $ zipWith edge poly (tail poly)
      where
        edge (ax, ay) (bx, by)
          | ax == bx = Right (ax, min ay by, max ay by)
          | otherwise = Left (ay, min ax bx, max ax bx)

    -- rectangle bounds check
    checkRect :: (Int, Int, Int, Int) -> Bool
    checkRect (ax, ay, bx, by) = not $ any crossH hSlice || any crossV vSlice
      where
        crossH (_, x1, x2) = (x1 <= ax && ax < x2) || (x1 < bx && bx <= x2)
        crossV (_, y1, y2) = (y1 <= ay && ay < y2) || (y1 < by && by <= y2)
        hSlice = filter (\(y, _, _) -> y > ay && y <= by) hSorted
        vSlice = filter (\(x, _, _) -> x > ax && x <= bx) vSorted
