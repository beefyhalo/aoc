import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- map digitToInt <$> readFile "input/2017/01.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve [[1,1,2,2], [1,1,1,1], [1,2,3,4], [9,1,2,1,2,1,2,9]]
-- [3,4,0,9]
solve, partTwo :: [Int] -> Int
solve xs = sum [a | (a, b) <- zip xs (drop 1 (cycle xs)), a == b]
partTwo xs = sum [a | (a, b) <- zip xs (drop n (cycle xs)), a == b]
  where
    n = length xs `div` 2
