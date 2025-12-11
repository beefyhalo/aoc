module Main (main) where

-- $setup
-- >>> input = "1"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2025/09.txt"
  print $ solve input

parse :: String -> Int
parse _ = 123

-- >>> solve example
solve :: [Int] -> Int
solve = undefined