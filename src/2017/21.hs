{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

import Data.AffineSpace (AffineSpace (..))
import Data.Constraint (Dict (Dict), withDict)
import Data.Foldable (toList)
import Data.Functor.Rep (index, tabulate)
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, SomeNat (SomeNat), natVal, type (<=))
import qualified GHC.TypeNats as GHC
import SizedGrid
import Unsafe.Coerce (unsafeCoerce)

type G n = Grid '[HardWrap n, HardWrap n] Char

data RuleMaps = RM
  { _rules23 :: Map String (G 3),
    _rules34 :: Map String (G 4)
  }

data SomeGrid where
  SomeGrid :: (KnownNat n, 1 <= n, KnownNat (n GHC.* n)) => G n -> SomeGrid


main :: IO ()
main = do
  input <- parse <$> readFile "input/2017/21.txt"
  let Just start = gridFromList @'[HardWrap 3, HardWrap 3] [".#.", "..#", "###"]
  print $ solve input start

parse :: String -> RuleMaps
parse s = RM (mk @2 @3) (mk @3 @4)
  where
    raw = [(splitOn "/" a, splitOn "/" b) | [a, "=>", b] <- words <$> lines s]
    mk :: forall n m. (KnownNat n, KnownNat m, KnownNat (n GHC.* n), KnownNat (m GHC.* m), 1 <= n) => Map String (G m)
    mk = M.fromList $ do
      (lhs, rhs) <- raw
      case (gridFromList @'[HardWrap n, HardWrap n] lhs, gridFromList @'[HardWrap m, HardWrap m] rhs) of
        (Just i, Just o) -> [(keyGrid v, o) | v <- variants @n i]
        _ -> []

solve :: (KnownNat n, 1 <= n) => RuleMaps -> G n -> (Int, Int)
solve input start = (countOn (iters !! 5), countOn (iters !! 18))
  where
    iters = iterate (stepSome input) (SomeGrid start)
    countOn (SomeGrid g) = length $ filter (== '#') (toList g)

variants :: forall n. (KnownNat n, 1 <= n) => G n -> [G n]
variants g = take 4 (iterate rotate g) ++ take 4 (iterate rotate (flipX g))
  where
    flipX, rotate :: G n -> G n
    flipX gr = tabulate $ \(x :| y :| EmptyCoord) ->
      let nVal = fromIntegral (natVal (Proxy @n)) - 1
       in index gr (toEnum (nVal - fromEnum x) :| y :| EmptyCoord)
    rotate = transposeGrid . flipX

-- | Converts a grid to the "a/b/c" string format used as Map keys
keyGrid :: (KnownNat n, KnownNat (n GHC.* n)) => G n -> String
keyGrid = intercalate "/" . collapseGrid

bundle ::
  forall n k.
  (KnownNat n, KnownNat k, KnownNat (n GHC.* k), 1 <= k, 1 <= n, 1 <= n GHC.* k) =>
  G (n GHC.* k) -> Grid '[HardWrap k, HardWrap k] (G n)
bundle g = tabulate $ \(rk :| ck :| EmptyCoord) ->
  tabulate $ \(ri :| ci :| EmptyCoord) ->
    let nVal = fromIntegral (natVal (Proxy @n))
        r = fromEnum rk * nVal + fromEnum ri
        c = fromEnum ck * nVal + fromEnum ci
     in index g (toEnum r :| toEnum c :| EmptyCoord)

-- | Stitching: Flatten nested blocks back into a single 2D grid
unbundle ::
  forall m k.
  (KnownNat m, KnownNat k, KnownNat (m GHC.* k), 1 <= m, 1 <= k, 1 <= m GHC.* k) =>
  Grid '[HardWrap k, HardWrap k] (G m) -> G (m GHC.* k)
unbundle blocks = tabulate $ \(i :| j :| EmptyCoord) ->
  let mVal = fromIntegral (natVal (Proxy @m))
      (bR, iR) = fromEnum i `divMod` mVal
      (bC, iC) = fromEnum j `divMod` mVal
   in index (index blocks (toEnum bR :| toEnum bC :| EmptyCoord)) (toEnum iR :| toEnum iC :| EmptyCoord)

-- | The core iteration logic
step ::
  forall n k m.
  ( KnownNat n,
    KnownNat k,
    KnownNat m,
    KnownNat (n GHC.* n),
    KnownNat (n GHC.* k),
    KnownNat (m GHC.* k),
    1 <= n,
    1 <= k,
    1 <= m,
    1 <= n GHC.* k,
    1 <= m GHC.* k
  ) =>
  Map String (G m) -> G (n GHC.* k) -> G (m GHC.* k)
step rules = unbundle @m @k . fmap apply . bundle @n @k
  where
    apply = (rules M.!) . keyGrid

withTrust :: forall c r. ((c) => r) -> r
withTrust = withDict (unsafeCoerce (Dict @(1 <= 1)) :: Dict c)

stepSome :: RuleMaps -> SomeGrid -> SomeGrid
stepSome (RM r23 r34) (SomeGrid (g :: G n))
  | even size = case GHC.someNatVal (size `div` 2) of
      SomeNat (Proxy :: Proxy k) -> withTrust @(n ~ 2 GHC.* k) $ SomeGrid (step @2 r23 g)
  | otherwise = case GHC.someNatVal (size `div` 3) of
      SomeNat (Proxy :: Proxy k) -> withTrust @(n ~ 3 GHC.* k) $ SomeGrid (step @3 r34 g)
  where
    size = natVal (Proxy @n)
