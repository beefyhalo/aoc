{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Data.List (sort)
import Data.List.Split (splitOn)

-- $setup
-- >>> input = "2x3x4\n1x1x10"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2015/02.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> (Int, Int, Int)
parse s = case splitOn "x" s of
  (sort . map read -> [l, w, h]) -> (l, w, h)

-- >>> solve example
-- 101
solve, partTwo :: [(Int, Int, Int)] -> Int
solve = sum . map volume
  where
    volume (l, w, h) = 2 * (l * w + w * h + h * l) + l * w

-- >>> partTwo example
-- 48
partTwo = sum . map volume
  where
    volume (l, w, h) = 2 * (l + w) + l * w * h
