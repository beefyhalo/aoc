{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Comonad.Store (extract, peeks, seeks)
import Data.AffineSpace ((.+^))
import Data.Char (isAlpha, isSpace)
import Data.Foldable (toList)
import Data.List (elemIndex, unfoldr)
import Data.Maybe (catMaybes)
import GHC.TypeNats (KnownNat, type (<=))
import qualified GHC.TypeNats as GHC
import SizedGrid

data Cell = Path | Turn | Letter Char | Empty deriving (Eq, Show)

data Dir = N | S | E | W

-- $setup
-- >>> input = "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "
-- >>> let padRows rs = take 16 (rs ++ repeat (replicate 16 ' '))
-- >>> Just (startFocus -> example) = parse @16 (unlines $ padRows $ lines input)

main :: IO ()
main = do
  Just (startFocus -> input) <- parse @201 <$> readFile "input/2017/19.txt"
  print $ solve input

parse :: forall n. (KnownNat n, KnownNat (n GHC.* n)) => String -> Maybe (Grid '[HardWrap n, HardWrap n] Cell)
parse = gridFromList . map (map parseCell) . lines
  where
    parseCell c
      | isAlpha c = Letter c
      | c == '+' = Turn
      | isSpace c = Empty
      | otherwise = Path

startFocus :: forall n. (KnownNat n, 1 <= n) => Grid '[HardWrap n, HardWrap n] Cell -> FocusedGrid '[HardWrap n, HardWrap n] Cell
startFocus g = FocusedGrid {focusedGrid = g, focusedGridPosition = mempty .+^ (0, toInteger col)}
  where
    Just col = elemIndex Path (toList g)

-- >>> solve example
-- ("ABCDEF",38)
solve :: (KnownNat n, 1 <= n) => FocusedGrid '[HardWrap n, HardWrap n] Cell -> (String, Int)
solve g = (catMaybes stream, length stream)
  where
    stream = unfoldr step (g, S)

orthogonal :: Dir -> [Dir]
orthogonal = \case
  N -> [E, W]
  S -> [E, W]
  E -> [N, S]
  W -> [N, S]

shift :: (KnownNat n, 1 <= n) => Dir -> Coord '[HardWrap n, HardWrap n] -> Coord '[HardWrap n, HardWrap n]
shift d g =
  g .+^ case d of
    N -> (-1, 0)
    S -> (1, 0)
    E -> (0, 1)
    W -> (0, -1)

type State n = (FocusedGrid '[HardWrap n, HardWrap n] Cell, Dir)

step :: (KnownNat n, 1 <= n) => State n -> Maybe (Maybe Char, State n)
step (g, dir) = case extract g of
  Empty -> Nothing
  cell -> Just (val, (nextGrid, nextDir))
    where
      val = case cell of Letter c -> Just c; _ -> Nothing
      nextDir = case cell of
        Turn -> head [d | d <- orthogonal dir, peeks (shift d) g /= Empty]
        _ -> dir
      nextGrid = seeks (shift nextDir) g
