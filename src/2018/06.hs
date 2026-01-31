{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.AffineSpace ((.-.))
import Data.Constraint (Dict (Dict), withDict)
import Data.Foldable (toList)
import Data.Functor.Rep (index, tabulate)
import qualified Data.IntMap.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, SomeNat (..), someNatVal, type (<=))
import SizedGrid
import Unsafe.Coerce (unsafeCoerce)

manhattan ::
  (KnownNat w, KnownNat h, 1 <= w, 1 <= h) =>
  Coord '[HardWrap w, HardWrap h] ->
  Coord '[HardWrap w, HardWrap h] ->
  Integer
manhattan c1 c2 = sum [dx1, dx2, dy1, dy2]
  where
    (dx1, dy1) = c1 .-. c2
    (dx2, dy2) = c2 .-. c1

closestIndex :: (KnownNat w, KnownNat h, 1 <= w, 1 <= h) => Coord '[HardWrap w, HardWrap h] -> [Coord '[HardWrap w, HardWrap h]] -> Maybe Int
closestIndex c ts = case sortOn fst $ zip (map (manhattan c) ts) [0 ..] of
  (d1, _) : (d2, _) : _ | d1 == d2 -> Nothing
  (_, i) : _ -> Just i
  _ -> Nothing

-- $setup
-- >>> input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2018/06.txt"
  print $ solve input

parse :: String -> (Integer, Integer)
parse s = (a, b)
  where
    [a, b] = read <$> splitOn ", " s

-- >>> solve example
-- (17,90)
solve :: [(Integer, Integer)] -> (Int, Int)
solve pts = case (someNatVal mx, someNatVal my) of
  (Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h))) ->
    withTrust @(1 <= w, 1 <= h) $ run @w @h pts
  where
    mx = maximum (map fst pts) + 1
    my = maximum (map snd pts) + 1

run :: forall w h. (KnownNat w, KnownNat h, 1 <= w, 1 <= h) => [(Integer, Integer)] -> (Int, Int)
run raw = (largestArea, regionSum)
  where
    targets :: [Coord '[HardWrap w, HardWrap h]]
    targets =
      [ HardWrap (fromJust $ numToOrdinal x)
          :| HardWrap (fromJust $ numToOrdinal y)
          :| EmptyCoord
      | (x, y) <- raw
      ]

    ownerGrid :: Grid '[HardWrap w, HardWrap h] (Maybe Int)
    ownerGrid = tabulate (`closestIndex` targets)

    infiniteIds :: IntSet
    infiniteIds = S.fromList [pos | c <- allCoord, Just pos <- [index ownerGrid c], isBorder c]
      where
        isBorder (x :| y :| EmptyCoord) = x == minBound || x == maxBound || y == minBound || y == maxBound

    largestArea = maximum $ M.fromListWith (+) [(pos, 1) | Just pos <- toList ownerGrid, S.notMember pos infiniteIds]

    safeGrid :: Grid '[HardWrap w, HardWrap h] Bool
    safeGrid = tabulate (\c -> sum (map (manhattan c) targets) < 10000)
    regionSum = sum [1 | True <- toList safeGrid]

withTrust :: forall c r. ((c) => r) -> r
withTrust = withDict (unsafeCoerce (Dict @(1 <= 1)) :: Dict c)
