{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List.Split (wordsBy)
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli (SomeMod, chineseSomeMod, modulo)

newtype CRT = C {unCRT :: SomeMod} deriving (Show)

instance Semigroup CRT where C x <> C y = C $ fromJust $ chineseSomeMod x y

instance Monoid CRT where mempty = C (0 `modulo` 1)

-- $setup
-- >>> input = "Disc #1 has 5 positions; at time=0, it is at position 4.\nDisc #2 has 2 positions; at time=0, it is at position 1."
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/15.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> CRT
parse s = C $ -((p + i) `modulo` fromInteger n)
  where
    [i, n, _, p] = map read $ wordsBy (not . isDigit) s

-- >>> solve example
-- >>> partTwo example
-- 5
-- 85
solve, partTwo :: [CRT] -> SomeMod
solve = unCRT . mconcat
partTwo d = solve (extra : d)
  where
    extra = C $ -(toInteger (length d + 1) `modulo` 11)
