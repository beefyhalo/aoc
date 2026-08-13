{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Comonad.Store (extract, seeks)
import Data.AffineSpace ((.+^))
import Data.Maybe (fromJust, isNothing, mapMaybe)
import GHC.TypeLits (KnownNat, type (<=))
import SizedGrid (Clamped, Coord, FocusedGrid (..), coordFromTuple, gridFromList)

data Move = U | D | L | R

-- A displacement is a Coord of the axes' Diffs, so it is built rather than
-- written as a tuple literal; coordFromTuple is the short way to say it.
delta :: Move -> Coord '[Int, Int]
delta = \case
  U -> coordFromTuple (-1, 0)
  D -> coordFromTuple (1, 0)
  L -> coordFromTuple (0, -1)
  R -> coordFromTuple (0, 1)

-- $setup
-- >>> input = "ULL\nRRDDD\nLURDL\nUUUUD"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/02.txt"
  print $ solve keypad input
  print $ solve keypad2 input

parse :: String -> [Move]
parse = map parseMove
  where
    parseMove 'U' = U
    parseMove 'D' = D
    parseMove 'L' = L
    parseMove 'R' = R

-- >>> solve keypad example
-- >>> solve keypad2 example
-- "1985"
-- "5DB3"
solve :: (KnownNat n, 1 <= n) => FocusedGrid '[Clamped n, Clamped n] (Maybe Char) -> [[Move]] -> String
solve start = mapMaybe extract . drop 1 . scanl (foldl' move) start
  where
    move fg c =
      let next = seeks (.+^ delta c) fg
       in if isNothing $ extract next then fg else next -- It's a "wall", stay put

keypad :: FocusedGrid '[Clamped 3, Clamped 3] (Maybe Char)
keypad =
  FocusedGrid
    { focusedGrid = fmap Just . fromJust $ gridFromList ["123", "456", "789"],
      focusedGridPosition = mempty .+^ coordFromTuple (1, 1)
    }

keypad2 :: FocusedGrid '[Clamped 5, Clamped 5] (Maybe Char)
keypad2 =
  FocusedGrid
    { focusedGrid =
        fromJust $
          gridFromList
            [ [Nothing, Nothing, Just '1', Nothing, Nothing],
              [Nothing, Just '2', Just '3', Just '4', Nothing],
              [Just '5', Just '6', Just '7', Just '8', Just '9'],
              [Nothing, Just 'A', Just 'B', Just 'C', Nothing],
              [Nothing, Nothing, Just 'D', Nothing, Nothing]
            ],
      focusedGridPosition = mempty .+^ coordFromTuple (2, 0)
    }
