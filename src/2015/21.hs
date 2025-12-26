{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.List (partition, subsequences)
import Data.Monoid (Sum (..))

type Player = (Int, Int, Int) -- hp, dmg, armor

type Item = (Int, Int, Int) -- cost, dmg, armor

weapons, armors, rings :: [Item]
weapons = [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]
armors = [(0, 0, 0), (13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]
rings = [(25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]

main :: IO ()
main = do
  boss <- parse <$> readFile "input/2015/21.txt"
  let (wins, loses) = games boss
  print $ minimum wins
  print $ maximum loses

parse :: String -> Player
parse s = (hp, d, a)
  where
    [hp, d, a] = map read $ filter (all isDigit) (words s)

games :: Player -> ([Item], [Item])
games boss = partition (\(_, d, a) -> playerWins (100, d, a) boss) setups
  where
    setups = map score allLoadouts
    allLoadouts = [w : a : rs | w <- weapons, a <- armors, rs <- subsequences rings, length rs <= 2]

playerWins :: Player -> Player -> Bool
playerWins (pHp, pD, pA) (bHp, bD, bA) = turns pHp (atk bD pA) > turns bHp (atk pD bA)
  where
    atk atkVal defVal = max 1 (atkVal - defVal)
    turns hp dmg = (hp + dmg - 1) `div` dmg

-- >>> score [(1, 1, 1), (1, 2, 3)]
-- (2,3,4)
score :: [Item] -> (Int, Int, Int)
score = coerce . foldMap (\(c, d, a) -> (Sum c, Sum d, Sum a))
