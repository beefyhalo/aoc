{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (foldl')
import Data.Maybe (fromJust)

-- $setup
-- >>> input = L.lines "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"

main :: IO ()
main = do
  input <- map parse . L.lines <$> L.readFile "input/2025/01.txt"
  print $ solve input
  print $ partTwo input

-- $setup
-- >>> example = map parse input

parse :: L.ByteString -> Int
parse bs = case L.uncons bs of
  Just ('L', rest) -> negate $ fst $ fromJust $ L.readInt rest
  Just ('R', rest) -> fst $ fromJust $ L.readInt rest
  _ -> 0

-- >>> solve example
-- 3
solve :: [Int] -> Int
solve = snd . foldl' go (50, 0)
  where
    go :: (Int, Int) -> Int -> (Int, Int)
    go (!pos, !count) n =
      let nextPos = (pos + n) `mod` 100
          nextCount = if nextPos == 0 then count + 1 else count
       in (nextPos, nextCount)

-- >>> partTwo example
-- 6
partTwo :: [Int] -> Int
partTwo = snd . foldl' go (50, 0)
  where
    go (!pos, !count) move =
      let zeroAdjust = if move < 0 then 1 else 0
          hits = abs (((pos - zeroAdjust) `div` 100) - ((pos + move - zeroAdjust) `div` 100))
          nextPos = pos + move
       in (nextPos, count + hits)
