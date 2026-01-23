{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (sortOn)
import Data.List.Split (splitOn)

-- $setup
-- >>> input = "0: 3\n1: 2\n4: 4\n6: 4"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2017/13.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> (Int, Int)
parse s = (read d, read r)
  where
    [d, r] = splitOn ":" s

-- >>> solve example
-- >>> partTwo example
-- 24
-- 10
solve, partTwo :: [(Int, Int)] -> Int
solve fw = sum [d * r | (d, r) <- fw, caught 0 (d, r)]
partTwo fw = head [delay | delay <- [0 ..], not . any (caught delay) $ sortOn snd fw]

caught :: Int -> (Int, Int) -> Bool
caught delay (d, r) = (d + delay) `mod` period == 0
  where
    period = 2 * (r - 1)
