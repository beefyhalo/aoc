{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Either (partitionEithers)
import Data.List (sort)
import qualified Data.Map.Lazy as M

data Target = Bot Int | Out Int deriving (Eq, Ord, Show)

data Rule = Rule {_low, _high :: Target}

-- $setup
-- >>> input = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- chips . parse . lines <$> readFile "input/2016/10.txt"
  print $ solve input
  print $ partTwo input

parse :: [String] -> (M.Map Int Rule, [(Target, [Int])])
parse ls = (M.fromList rules, inputs)
  where
    (rules, inputs) = partitionEithers $ map parseLine ls

    parseLine l = case words l of
      ["value", n, _, _, _, to] -> Right (Bot (read to), [read n])
      ["bot", b, _, _, _, lt, lo, _, _, _, ht, hi] -> Left (read b, Rule (parseDest lt lo) (parseDest ht hi))

    parseDest "bot" n = Bot (read n)
    parseDest "output" n = Out (read n)

solve, partTwo :: M.Map Target [Int] -> Int
solve cs = head [b | (Bot b, coins) <- M.toList cs, sort coins == [17, 61]]
partTwo cs = product [head $ cs M.! Out n | n <- [0, 1, 2]]

-- >>> chips example
-- fromList [(Bot 0,[5,3]),(Bot 1,[2,3]),(Bot 2,[2,5]),(Out 0,[5]),(Out 1,[2]),(Out 2,[3])]
chips :: (M.Map Int Rule, [(Target, [Int])]) -> M.Map Target [Int]
chips (rules, initial) = chipMap
  where
    chipMap = M.fromListWith (++) (initial ++ transfers)
    transfers =
      concat
        [ [(l, [minVal]), (h, [maxVal])]
        | (b, Rule l h) <- M.toList rules,
          let [minVal, maxVal] = sort $ chipMap M.! Bot b
        ]
