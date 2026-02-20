import Data.List (partition)

type Point = [Int] -- 4 element list

manhattan :: Point -> Point -> Int
manhattan p = sum . zipWith ((abs .) . (-)) p

-- $setup
-- >>> input = " 0,0,0,0\n3,0,0,0\n0,3,0,0\n0,0,3,0\n0,0,0,3\n0,0,0,6\n9,0,0,0\n12,0,0,0"
-- >>> example = map parse (lines input)

main :: IO ()
main = print . solve . map parse . lines =<< readFile "input/2018/25.txt"

parse :: String -> Point
parse s = read $ "[" ++ s ++ "]"

-- >>> solve example
-- 2
solve :: [Point] -> Int
solve = length . merge

merge :: [Point] -> [[Point]]
merge = foldl' insert []
  where
    insert :: [[Point]] -> Point -> [[Point]]
    insert cs p = (p : concat close) : far
      where
        (close, far) = partition (any ((3 >=) . manhattan p)) cs
