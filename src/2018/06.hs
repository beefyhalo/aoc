{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Lens (ifoldl')
import Data.AffineSpace ((.+^), (.-.))
import Data.Foldable (toList)
import Data.Functor.Rep (index, tabulate)
import qualified Data.IntMap.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import GHC.TypeNats (KnownNat, type (<=))
import SizedGrid

manhattan ::
  (KnownNat w, KnownNat h, 1 <= w, 1 <= h) =>
  Coord '[Clamped w, Clamped h] -> Coord '[Clamped w, Clamped h] -> Int
manhattan c1 c2 = fromIntegral $ sum $ map abs [dx, dy]
  where
    (dx, dy) = coordToTuple (c1 .-. c2)

-- $setup
-- >>> input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2018/06.txt"
  print $ solve @400 @400 input

parse :: (KnownNat w, KnownNat h, 1 <= w, 1 <= h) => String -> [Coord '[Clamped w, Clamped h]]
parse = map (toCoord . splitOn ", ") . lines
  where
    toCoord [a, b] = mempty .+^ coordFromTuple (read a, read b)

-- >>> solve @9 @10 example
-- (17,90)
solve :: forall w h. (KnownNat w, KnownNat h, 1 <= w, 1 <= h) => [Coord '[Clamped w, Clamped h]] -> (Int, Int)
solve targets = (maximum areas, sum regions)
  where
    ownerGrid :: Grid '[Clamped w, Clamped h] (Maybe Int)
    ownerGrid = tabulate (closestIndex targets)
      where
        closestIndex ts c
          | tie = Nothing
          | otherwise = Just best
          where
            (best, _, tie) = ifoldl' step (0, maxBound, False) ts
            step i (bi, bd, ti) (manhattan c -> d) = case compare d bd of
              LT -> (i, d, False)
              EQ -> (bi, bd, True)
              GT -> (bi, bd, ti)

    inf :: IntSet
    inf = S.fromList $ mapMaybe (index ownerGrid) border
      where
        border =
          [x :| y :| EmptyCoord | x <- [minBound .. maxBound], y <- [minBound, maxBound]] ++
          [x :| y :| EmptyCoord | x <- [minBound, maxBound], y <- [minBound .. maxBound]]

    areas = M.fromListWith (+) [(pos, 1) | Just pos <- toList ownerGrid, S.notMember pos inf]

    regions :: Grid '[Clamped w, Clamped h] Int
    regions = tabulate checkSafe
      where
        checkSafe c = fromEnum $ sum (manhattan c <$> targets) < 10000
