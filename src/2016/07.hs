{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (tails)
import Data.List.Split (wordsBy)

-- $setup
-- >>> input = "abba[mnop]qrst\naba[bab]xyz"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/07.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> ([String], [String])
parse = foldr (\x (o, h) -> (x : h, o)) ([], []) . wordsBy (`elem` "[]")

-- >>> example
-- >>> solve example
-- >>> partTwo example
-- [(["abba","qrst"],["mnop"]),(["aba","xyz"],["bab"])]
-- 1
-- 1
solve, partTwo :: [([String], [String])] -> Int
solve = length . filter supportsTLS
partTwo = length . filter supportsSSL

supportsTLS, supportsSSL :: ([String], [String]) -> Bool
supportsTLS (o, h) = any hasABBA o && not (any hasABBA h)
  where
    hasABBA = any (\[a, b, c, d] -> a == d && b == c && a /= b) . windows 4
supportsSSL (o, h) = or [[b, a, b] `elem` concatMap abas h | [a, b, _] <- concatMap abas o]
  where
    abas = filter (\[a, b, c] -> a == c && a /= b) . windows 3

windows :: Int -> [a] -> [[a]]
windows n xs = [take n t | t <- tails xs, length t >= n]
