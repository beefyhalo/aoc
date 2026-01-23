{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- HLINT ignore "Use underscore" -}

import Control.Arrow ((***))
import Data.Bits (xor, (.&.))

main :: IO ()
main = do
  input <- parse . words <$> readFile "input/2017/15.txt"
  print $ solve 40_000_000 input
  print $ partTwo 5_000_000 input

parse :: [String] -> (Int, Int)
parse ws = (read $ ws !! 4, read $ ws !! 9)

-- >>> solve 40_000_000 65 8921
-- >>> partTwo 5_000_000 65 8921
-- 588
-- 309
solve, partTwo :: Int -> (Int, Int) -> Int
solve = check 1 1
partTwo = check 4 8

check :: Int -> Int -> Int -> (Int, Int) -> Int
check mult1 mult2 limit start =
  sum
    [ 1
    | (a, b) <- take limit $ iterate (next 16807 mult1 *** next 48271 mult2) start,
      (a `xor` b) .&. 0xFFFF == 0
    ]

next :: Int -> Int -> Int -> Int
next f m !x = let y = f * x `mod` 2147483647 in if y `rem` m == 0 then y else next f m y
