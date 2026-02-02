{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (mapAccumL)

main :: IO ()
main = do
  input <- parse <$> readFile "input/2018/08.txt"
  print $ solve input

parse :: String -> [Int]
parse = map read . words

-- $setup
-- >>> input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
-- >>> example = parse input

-- >>> solve example
-- (138,66)
solve :: [Int] -> (Int, Int)
solve = snd . node

node :: [Int] -> ([Int], (Int, Int))
node (c : m : xs) = (rest, (sumMeta, val))
  where
    (childMeta, childVals, xs') = children c xs
    (meta, rest) = splitAt m xs'
    sumMeta = childMeta + sum meta
    val
      | c == 0 = sum meta
      | otherwise = sum [childVals !! (i - 1) | i <- meta, i <= c]

children :: Int -> [Int] -> (Int, [Int], [Int])
children n xs = (sum weights, values, rest)
  where
    (rest, results) = mapAccumL (const . node) xs [1 .. n]
    (weights, values) = unzip results
