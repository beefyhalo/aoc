{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

main :: IO ()
main = do
  input <- lines <$> readFile "input/2015/05.txt"
  print $ solve input
  print $ partTwo input

solve, partTwo :: [String] -> Int
solve = length . filter isNice
partTwo = length . filter isNice2

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

-- >>> map isNice ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"]
-- [True,True,False,False,False]
isNice :: String -> Bool
isNice s = hasVowels && hasEquals && noIllegal
  where
    ps = pairs s
    vowels = "aeiou"
    illegal = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]
    hasVowels = length (filter (`elem` vowels) s) >= 3
    hasEquals = any (uncurry (==)) ps
    noIllegal = not (any (`elem` illegal) ps)

-- >>> map isNice2 ["qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy"]
-- [True,True,False,False]
isNice2 :: String -> Bool
isNice2 s = hasPair && hasRepeat
  where
    ps = pairs s
    hasPair = or $ zipWith (\i p -> p `elem` drop (i + 1) ps) [0 ..] ps
    hasRepeat = or $ zipWith3 (\a _ c -> a == c) s (drop 1 s) (drop 2 s)
