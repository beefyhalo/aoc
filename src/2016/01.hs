{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split (splitOn)
import Data.Monoid (Sum (..))
import qualified Data.Set as S

type Pos = (Sum Int, Sum Int)

type Dir = (Sum Int, Sum Int) -- (dx, dy)

data Turn = L | R deriving (Show, Eq)

rot :: Turn -> Dir -> Dir
rot L (x, y) = (-y, x)
rot R (x, y) = (y, -x)

-- $setup
-- >>> input = "R5, L5, R5, R3"
-- >>> example = map parse (splitOn ", " input)

main :: IO ()
main = do
  input <- map parse . splitOn ", " <$> readFile "input/2016/01.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> (Turn, Int)
parse ('L' : n) = (L, read n)
parse ('R' : n) = (R, read n)

-- >>> solve example
-- 12
solve, partTwo :: [(Turn, Int)] -> Int
solve = manhattan . last . fullPath
partTwo = maybe 0 manhattan . firstDup . fullPath

fullPath :: [(Turn, Int)] -> [Pos]
fullPath instrs = scanl (<>) (0, 0) steps
  where
    dirs = drop 1 $ scanl (\d (t, _) -> rot t d) (0, 1) instrs
    steps = concat $ zipWith replicate (map snd instrs) dirs

manhattan :: Pos -> Int
manhattan (x, y) = getSum $ abs x + abs y

firstDup :: [Pos] -> Maybe Pos
firstDup = go S.empty
  where
    go _ [] = Nothing
    go seen (p : ps)
      | p `S.member` seen = Just p
      | otherwise = go (S.insert p seen) ps
