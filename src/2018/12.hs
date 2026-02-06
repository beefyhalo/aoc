{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import qualified Data.IntSet as S
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import Data.MonoTraversable (osum)

type Pots = S.IntSet

type Rules = M.Map [Bool] Bool

-- $setup
-- >>> input = "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"
-- >>> example = parse (lines input)

main :: IO ()
main = do
  example <- parse . lines <$> readFile "input/2018/12.txt"
  print $ solve example

parse :: [String] -> (Pots, Rules)
parse (start : _ : rest) = (pots, rules)
  where
    pots = S.fromList [i | (i, '#') <- zip [0 ..] (drop 15 start)]
    rules = M.fromList [(map (== '#') pat, True) | l <- rest, [pat, "=>", "#"] <- [words l]]

-- >>> solve example
-- (325,999999999374)
solve :: (Pots, Rules) -> (Int, Int)
solve (start, rules) = (scores !! 20, s + (50000000000 - g) * d)
  where
    scores = osum <$> iterate (step rules) start
    diffs = zipWith (-) (tail scores) scores
    Just i = findIndex (\(a, b, c) -> a == b && b == c) $ zip3 diffs (tail diffs) (drop 2 diffs)
    g = i + 1
    s = scores !! g
    d = diffs !! i

step :: Rules -> Pots -> Pots
step rules pots =
  S.fromList
    [ i
    | i <- [lo - 2 .. hi + 2],
      let pat = [S.member (i + d) pots | d <- [-2 .. 2]],
      M.findWithDefault False pat rules
    ]
  where
    lo = S.findMin pots
    hi = S.findMax pots
