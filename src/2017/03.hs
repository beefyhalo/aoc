{-# OPTIONS_GHC -Wno-x-partial #-}

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- read <$> readFile "input/2017/03.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve [1, 12, 23, 1024]
-- >>> map partTwo [1, 2, 3, 4, 5]
-- [0,3,2,31]
-- [2,4,4,5,10]
solve, partTwo :: Int -> Int
solve n = layer + minimum [abs (n - c) | c <- centers]
  where
    layer = ceiling ((sqrt (fromIntegral n) - 1) / 2 :: Float)
    maxLayer = (2 * layer + 1) ^ (2 :: Int)
    centers = [maxLayer - layer - 2 * layer * i | i <- [0 .. 3]]
partTwo n = head [v | v <- spiral, v > n]

dirs :: [(Int, Int)]
dirs = concat $ zipWith replicate lengths (cycle [(1, 0), (0, 1), (-1, 0), (0, -1)])
  where
    lengths = concatMap (replicate 2) [1 ..]

spiral :: [Int]
spiral = [v | (_, _, v) <- scanl step ((0, 0), M.singleton (0, 0) 1, 1) dirs]
  where
    step ((x, y), m, _) (dx, dy) =
      let pos = (x + dx, y + dy)
          v = sum [M.findWithDefault 0 n m | n <- neighbors pos]
       in (pos, M.insert pos v m, v)

    neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]
