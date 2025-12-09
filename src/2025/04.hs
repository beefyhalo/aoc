{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Comonad.Store (Comonad (extract), ComonadStore (experiment), extend, pos)
import Control.Lens (FoldableWithIndex (ifoldMap'), view)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (unfoldr)
import Data.Monoid (getSum)
import GHC.TypeLits (KnownNat, type (<=))
import qualified GHC.TypeLits as GHC
import SizedGrid

data Cell = Paper | Empty deriving (Eq, Show)

-- $setup
-- >>> input = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."
-- >>> Just (view asFocusedGrid -> example) = parse @10 input

main :: IO ()
main = do
  Just (view asFocusedGrid -> input) <- parse @136 <$> readFile "input/2025/04.txt"
  print $ solve input
  print $ partTwo input

-- >>> parse @10 input
-- Just (Grid {unGrid = [Empty,Empty,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Empty,Paper,Empty,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Empty]})
parse :: (KnownNat n, KnownNat (n GHC.* n)) => String -> Maybe (Grid '[HardWrap n, HardWrap n] Cell)
parse = gridFromList . map (map charToCell) . lines
  where
    charToCell '@' = Paper
    charToCell _ = Empty

-- >>> solve example
-- 71
solve, partTwo :: (KnownNat n, 1 <= n) => FocusedGrid '[HardWrap n, HardWrap n] Cell -> Int
solve g = getSum $ ifoldMap' go (view asGrid g)
  where
    go coord = \case
      Paper | forklift g coord -> 1
      _ -> 0

-- >>> partTwo example
-- 43
partTwo = sum . unfoldr go
  where
    go g
      | removed == 0 = Nothing
      | otherwise = Just (removed, g')
      where
        g' = extend (\c -> if forklift g (pos c) then Empty else extract c) g
        removed = on (-) (length . filter (== Paper) . toList) g g'

forklift :: (KnownNat n, 1 <= n) => FocusedGrid '[HardWrap n, HardWrap n] Cell -> Coord '[HardWrap n, HardWrap n] -> Bool
forklift g coord =
  let neighs = experiment (filter (/= coord) . nubOrd . moorePoints 1) g
      papers = length $ filter (== Paper) neighs
   in papers < 4
