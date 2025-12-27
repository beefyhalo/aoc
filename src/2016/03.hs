import Data.List (transpose)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> readFile "input/2016/03.txt"
  print $ solve input
  print $ partTwo input

solve, partTwo :: [[Int]] -> Int
solve = length . filter isTriangle

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = a + b > c && a + c > b && b + c > a
isTriangle _ = False

partTwo = solve . concatMap transpose . chunksOf 3