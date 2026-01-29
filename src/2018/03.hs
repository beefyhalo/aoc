{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Array.Unboxed (UArray, accumArray, elems, (!))
import Data.Char (isDigit)
import Data.List (find)
import Data.List.Split (wordsBy)

data Claim = Claim {claimId, left, top, width, height :: Int}

-- $setup
-- >>> input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2018/03.txt"
  print $ solve input

parse :: String -> Claim
parse line = Claim cid l t w h
  where
    [cid, l, t, w, h] = map read $ wordsBy (not . isDigit) line

-- >>> solve example
-- (4,3)
solve :: [Claim] -> (Int, Int)
solve claims = (overlaps, claimId intact)
  where
    grid :: UArray (Int, Int) Int
    grid = accumArray (+) 0 ((0, 0), (999, 999)) $ (,1) <$> concatMap coords claims
    overlaps = length $ filter (> 1) (elems grid)
    Just intact = find (all (\pos -> grid ! pos == 1) . coords) claims

coords :: Claim -> [(Int, Int)]
coords Claim {..} =
  [(x, y) | x <- [left .. left + width - 1], y <- [top .. top + height - 1]]
