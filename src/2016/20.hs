{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (foldl1', sortOn)
import Data.List.Split (splitOn)

type Range = (Int, Int)

-- $setup
-- >>> example = [(5,8),(0,2),(4,7)]

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/20.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Range
parse s = let [a, b] = read <$> splitOn "-" s in (a, b)

-- >>> solve example
-- >>> partTwo example
-- 3
-- 4294967288
solve, partTwo :: [Range] -> Int
solve = (+ 1) . snd . head . merge
partTwo rs = gaps + 4294967295 - lastIP
  where
    (gaps, lastIP) = foldl1' step (merge rs)
    step (acc, cur) (s, e) = (acc + max 0 (s - cur), max cur (e + 1))

merge :: [Range] -> [Range]
merge = foldr go [] . sortOn fst
  where
    go r [] = [r]
    go (s, e) ((s', e') : rs)
      | e + 1 < s' = (s, e) : (s', e') : rs
      | otherwise = (min s s', max e e') : rs
