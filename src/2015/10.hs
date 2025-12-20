module Main (main) where

import Data.List (group)
import Data.Monoid (Endo (..))
import Data.Semigroup (stimes)

-- $setup
-- >>> example = ["1", "11", "21", "1211", "111221"]

main :: IO ()
main = do
  input <- readFile "input/2015/10.txt"
  print $ solve 40 input
  print $ solve 50 input

solve :: Int -> String -> Int
-- solve n = length . (!! n) . iterate step
-- solve n = length . last . take (n + 1) . iterate step
solve n = length . appEndo (stimes n (Endo step))

-- >>> map step example
-- ["11","21","1211","111221","312211"]
step :: String -> String
step = concatMap (\g -> show (length g) ++ [head g]) . group
