{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Containers.ListUtils (nubOrd)
import Data.List (inits, stripPrefix, tails, unfoldr)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

-- $setup
-- >>> replacements = [("H", "HO"), ("H", "OH"), ("O", "HH")]
-- >>> example = ["HOH", "HOHOHO"]

main :: IO ()
main = do
  (replacements, input) <- parse <$> readFile "input/2015/19.txt"
  print $ solve replacements input
  print $ partTwo replacements input

parse :: String -> ([(String, String)], String)
parse s = (reps, molecule)
  where
    ls = filter (not . null) $ lines s
    reps = [(a, b) | l <- init ls, let [a, b] = splitOn " => " l]
    molecule = last ls

-- >>> map (solve replacements) example
-- >>> map (partTwo replacements) example
-- [4,7]
-- [5,256]
solve, partTwo :: [(String, String)] -> String -> Int
solve reps = length . nubOrd . step reps

step :: [(String, String)] -> String -> [String]
step reps mol =
  [ pre ++ to ++ post
  | (pre, suf) <- reverse $ zip (inits mol) (tails mol),
    (from, to) <- reps,
    Just post <- [stripPrefix from suf]
  ]

partTwo reps mol = length $ unfoldr go [mol]
  where
    rev = map swap reps
    go (m : rs) | m /= "e" = Just (m, step rev m ++ rs)
    go _ = Nothing

-- Magic Formula Version

-- partTwo _ mol = total - rn - ar - (2 * y) - 1
--   where
--     -- Count capital letters to get total number of elements
--     total = length $ filter isUpper mol

--     -- Count specific tokens
--     count sub = length $ filter (isPrefixOf sub) (tails mol)
--     rn = count "Rn"
--     ar = count "Ar"
--     y = count "Y"
