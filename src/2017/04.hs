import Data.List (sort)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- map words . lines <$> readFile "input/2017/04.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve [["aa", "bb", "cc", "dd"]]
-- >>> partTwo [["oiii", "ioii", "iioi", "iiio"]]
-- 1
-- 0
solve, partTwo :: [[String]] -> Int
solve xs = length [p | p <- xs, S.size (S.fromList p) == length p]
partTwo = solve . map (map sort)
