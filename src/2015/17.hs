{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import qualified Data.IntMap.Strict as M
import Data.List (foldl')

-- $setup
-- >>> example = [20,15,10,5,5]

-- solve n = length . filter ((== n) . sum) . subsequences

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input/2015/17.txt"
  print $ solve 150 input
  print $ partTwo 150 input

-- >>> solve 25 example
-- >>> partTwo 25 example
-- 4
-- 3
solve, partTwo :: Int -> [Int] -> Int
solve n = sum . ways n
partTwo n = snd . M.findMin . ways n

ways :: Int -> [Int] -> M.IntMap Int
ways n = (!! n) . foldl' step start
  where
    start = M.singleton 0 1 : replicate n M.empty
    -- ways[s] = ways[s] + ways[s - c]
    -- We pad with 'c' zeros to shift the list and align (s - c) with s
    step dp c = zipWith (M.unionWith (+)) dp (replicate c M.empty ++ map (M.mapKeys (+ 1)) dp)
