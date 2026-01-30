import Data.Char (toLower)
import Data.Function (on)

main :: IO ()
main = do
  input <- readFile "input/2018/05.txt"
  print $ solve input

-- >>> solve "dabAcCaCBAcCcaDA"
-- (10,4)
solve :: String -> (Int, Int)
solve input = (length $ reduce input, shortest)
  where
    shortest = minimum [length $ reduce $ filter ((/= u) . toLower) input | u <- ['a' .. 'z']]

reduce :: String -> String
reduce = foldr step ""
  where
    step c [] = [c]
    step c (x : xs)
      | c /= x && on (==) toLower c x = xs
      | otherwise = c : x : xs
