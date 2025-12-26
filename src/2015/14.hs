{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List (foldl1', transpose)

data Reindeer = Reindeer {_speed, _flyTime, _restTime :: Int}

-- $setup
-- >>> input = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2015/14.txt"
  print $ solve 2503 input
  print $ partTwo 2503 input

parse :: String -> Reindeer
parse s = Reindeer v f r
  where
    [v, f, r] = map read $ filter (all isDigit) (words s)

-- >>> solve 1000 example
-- 1120
solve, partTwo :: Int -> [Reindeer] -> Int
solve n = maximum . map (\r -> reindeerStream r !! (n - 1))

reindeerStream :: Reindeer -> [Int]
reindeerStream (Reindeer v f r) = scanl1 (+) $ cycle (replicate f v ++ replicate r 0)

-- >>> partTwo 1000 example
-- 689
partTwo n rs = maximum $ foldl1' (zipWith (+)) $ map leaderMask perSecondDists
  where
    perSecondDists = take n $ transpose $ map reindeerStream rs

    leaderMask ds =
      let m = maximum ds
       in map (\d -> if d == m then 1 else 0) ds
