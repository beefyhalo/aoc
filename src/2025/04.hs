import Control.Comonad.Store (Comonad (extract), ComonadStore (experiment), extend, pos, seek)
import Control.Lens (FoldableWithIndex (ifoldMap'), view)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Grid.Sized
import Data.List (unfoldr)
import Data.Monoid (getSum)
import GHC.TypeLits (KnownNat, type (<=))

data Cell = Paper | Empty deriving (Eq, Show)

-- $setup
-- >>> input = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."
-- >>> Just ((`FocusedGrid` zeroCoord) -> example) = parse @10 input

main :: IO ()
main = do
  Just ((`FocusedGrid` zeroCoord) -> input) <- parse @136 <$> readFile "input/2025/04.txt"
  print $ solve input
  print $ partTwo input

-- >>> parse @10 input
-- Just (Grid {unGrid = [Empty,Empty,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Empty,Paper,Empty,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Empty,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Paper,Empty,Paper,Empty,Paper,Empty,Paper,Paper,Paper,Empty,Paper,Empty]})
parse :: (KnownNat n) => String -> Maybe (Grid '[Clamped n, Clamped n] Cell)
parse = gridFromList . map (map charToCell) . lines
  where
    charToCell '@' = Paper
    charToCell _ = Empty

-- >>> solve example
-- 13
solve, partTwo :: (KnownNat n, 1 <= n) => FocusedGrid '[Clamped n, Clamped n] Cell -> Int
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

forklift :: (KnownNat n, 1 <= n) => FocusedGrid '[Clamped n, Clamped n] Cell -> Coord '[Clamped n, Clamped n] -> Bool
forklift g coord = papers < 4
  where
    neighs = experiment neighbours (seek coord g)
    papers = length $ filter (== Paper) neighs
