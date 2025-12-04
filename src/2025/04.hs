{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Comonad.Store (Comonad (extract), ComonadStore, extend, pos)
import Control.Comonad.Store.Class (peek)
import Control.Lens (FoldableWithIndex (ifoldMap'), view)
import Data.AffineSpace (AffineSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (unfoldr)
import Data.Monoid (getSum)
import GHC.TypeLits (KnownNat)
import qualified GHC.TypeLits as GHC
import SizedGrid

data Cell = Paper | Empty deriving (Eq)

instance Show Cell where
  show Paper = "@"
  show Empty = " "

-- $setup
-- >>> input = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."
-- >>> Just (view asFocusedGrid -> example) = parse @10 input

main :: IO ()
main = do
  Just (view asFocusedGrid -> input) <- parse @136 <$> readFile "input/2025/04.txt"
  print $ solve input
  print $ partTwo input

charToCell :: Char -> Cell
charToCell '@' = Paper
charToCell _ = Empty

-- >>> parse @10 input
-- Just (Grid {unGrid = [ , ,@,@, ,@,@,@,@, ,@,@,@, ,@, ,@, ,@,@,@,@,@,@,@, ,@, ,@,@,@, ,@,@,@,@, , ,@, ,@,@, ,@,@,@,@, ,@,@, ,@,@,@,@,@,@,@, ,@, ,@, ,@, ,@, ,@,@,@,@, ,@,@,@, ,@,@,@,@, ,@,@,@,@,@,@,@,@, ,@, ,@, ,@,@,@, ,@, ]})
parse :: (KnownNat n, KnownNat (n GHC.* n)) => String -> Maybe (Grid '[HardWrap n, HardWrap n] Cell)
parse = gridFromList . map (map charToCell) . lines

-- >>> solve example
-- 13
solve ::
  ( AllSizedKnown cs,
    All IsCoordLifted cs,
    All AffineSpace cs,
    AllDiffSame Integer cs,
    All Eq cs,
    All Monoid cs,
    All Semigroup cs,
    All Ord cs
  ) =>
  FocusedGrid cs Cell ->
  Int
solve g = getSum $ ifoldMap' go (view asGrid g)
  where
    go coord = \case
      Paper | isAccessible g coord -> 1
      _ -> 0

isAccessible ::
  ( AllDiffSame Integer cs,
    All Eq cs,
    All Ord cs,
    All AffineSpace cs,
    ComonadStore (Coord cs) w
  ) =>
  w Cell ->
  Coord cs ->
  Bool
isAccessible g coord =
  let neighs = filter (/= coord) (nubOrd $ moorePoints (1 :: Integer) coord)
      papers = length $ filter (\p -> peek p g == Paper) neighs
   in papers < 4

-- >>> partTwo example
-- 43
partTwo ::
  ( AllSizedKnown cs,
    All Ord cs,
    All Semigroup cs,
    All Monoid cs,
    All Eq cs,
    All AffineSpace cs,
    All IsCoordLifted cs,
    AllDiffSame Integer cs
  ) =>
  FocusedGrid cs Cell ->
  Int
partTwo = sum . unfoldr go
  where
    go g
      | removed == 0 = Nothing
      | otherwise = Just (removed, g')
      where
        g' = extend (\fg -> if isAccessible g (pos fg) then Empty else extract fg) g
        removed = on (-) (length . filter (== Paper) . toList) g g'
