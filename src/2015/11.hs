module Main (main) where

import Data.List (group)

-- $setup
-- >>> example = ["hijklmmn", "abbceffg", "abbcegjk", "abcdefgh", "ghijklmn"]

main :: IO ()
main = do
  input <- readFile "input/2015/11.txt"
  let res = solve input
  putStrLn res
  putStrLn $ solve res

solve :: String -> String
solve = until valid succPwd . succPwd

-- >>> map succPwd example
-- ["hijklmmo","abbceffh","abbcegjl","abcdefgi","ghijklmo"]
succPwd :: String -> String
succPwd = snd . foldr step (True, [])
  where
    step c (carry, acc)
      | not carry = (False, c : acc)
      | c == 'z' = (True, 'a' : acc)
      | otherwise = (False, succ c : acc)

valid :: String -> Bool
valid s = straight && noConfusing && pairs
  where
    straight = or $ zipWith3 (\a b c -> succ a == b && succ b == c) s (tail s) (drop 2 s)
    noConfusing = all (`notElem` "iol") s
    pairs = length (filter ((>= 2) . length) (group s)) >= 2
