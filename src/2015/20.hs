{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Data.Array.Unboxed (UArray, accumArray, assocs)

main :: IO ()
main = do
  input <- read <$> readFile "input/2015/20.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve 70
-- 4
solve, partTwo :: Int -> Int
solve = solveWith 10 (\elf -> [elf, elf * 2 ..])
partTwo = solveWith 11 (\elf -> take 50 [elf, elf * 2 ..])

solveWith :: Int -> (Int -> [Int]) -> Int -> Int
solveWith mul gen n = head [i | (i, p) <- assocs presents, p >= n]
  where
    presents :: UArray Int Int
    presents = accumArray (+) 0 (1, limit) [(house, elf * mul) | elf <- [1 .. limit], house <- takeWhile (<= limit) (gen elf)]
    limit = n `div` 10
