module Main (main) where

import Data.List (find, sortOn)
import Data.Ord (Down (..))

-- $setup
-- >>> example = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input/2015/24.txt"
  print $ solve input 3
  print $ solve input 4

-- >>> solve example 3
-- >>> solve example 4
-- 99
-- 44
solve :: [Int] -> Int -> Int
solve weights groups = minimum $ map product $ bestGroups target sortedWeights
  where
    sortedWeights = sortOn Down weights
    target = sum weights `div` groups

-- Find the smallest k such that at least one combination exists
bestGroups :: Int -> [Int] -> [[Int]]
bestGroups target ws = concat $ find (not . null) [combos k target ws | k <- [1 .. length ws]]

-- >>> [c | k <- [1..10], c <- combos k 10 example]
-- [[10],[1,9],[2,8],[3,7],[1,2,7],[1,4,5],[2,3,5],[1,2,3,4]]
combos :: Int -> Int -> [Int] -> [[Int]]
combos k target xs = [cs | (cs, s) <- final, length cs == k, s == target]
  where
    step x acc = [(x : cs, s + x) | (cs, s) <- acc, length cs < k, s + x <= target] ++ acc
    final = foldr step [([], 0)] xs
