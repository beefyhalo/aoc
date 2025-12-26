{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Containers.ListUtils (nubOrd)
import Data.List (permutations)
import qualified Data.Map.Strict as M

type Person = String

type HappinessMap = M.Map (Person, Person) Int

-- $setup
-- >>> input = "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol."
-- >>> example = M.fromList $ map parse (lines input)

main :: IO ()
main = do
  input <- M.fromList . map parse . lines <$> readFile "input/2015/13.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> ((Person, Person), Int)
parse line = ((p1, p2), val)
  where
    [p1, _, gainStr, read -> units, _, _, _, _, _, _, init -> p2] = words line
    val = if gainStr == "gain" then units else -units

-- >>> solve example
-- >>> partTwo example
-- 330
-- 286
solve, partTwo :: HappinessMap -> Int
solve hmap = maximum $ map totalHappiness perms
  where
    totalHappiness xs = sum $ zipWith (\a b -> h a b + h b a) xs (drop 1 $ cycle xs)
    h x y = M.findWithDefault 0 (x, y) hmap
    (p : ps) = nubOrd $ concat [[a, b] | (a, b) <- M.keys hmap]
    perms = map (p :) (permutations ps)
partTwo = solve . M.insert ("ME", "ME") 0
