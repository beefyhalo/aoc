{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.List (group, tails)

-- $setup
-- >>> example = ["hijklmmn", "abbceffg", "abbcegjk", "abcdefgh", "ghijklmn"]

main :: IO ()
main = do
  input <- readFile "input/2015/11.txt"
  let res = solve input
  putStrLn res
  putStrLn $ solve (succPwd res)

solve :: String -> String
solve = until valid succPwd

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
valid s = straight s && noConfusing s && pairs s
  where
    straight = any ((\case [a, b, c] -> succ a == b && succ b == c; _ -> False) . take 3) . tails
    noConfusing = all (`notElem` "iol")
    pairs = (>= 2) . length . filter ((>= 2) . length) . group
