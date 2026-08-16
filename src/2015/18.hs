import Control.Comonad (extend, extract)
import Control.Comonad.Store (experiment, pos)
import Control.Lens (view)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Grid.Sized
import GHC.TypeLits (KnownNat, type (<=))

data Cell = Alive | Dead deriving (Eq, Show)

-- $setup
-- >>> input = ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
-- >>> Just example = view asFocusedGrid <$> parse @6 input

main :: IO ()
main = do
  Just (view asFocusedGrid -> input) <- parse @100 <$> readFile "input/2015/18.txt"
  print $ solve 100 input
  print $ partTwo 100 input

parse :: (KnownNat n) => String -> Maybe (Grid '[Clamped n, Clamped n] Cell)
parse = gridFromList . map (map (bool Dead Alive . (== '#'))) . lines

-- >>> solve 2 example
-- >>> partTwo 2 example
-- 8
-- 14
solve, partTwo :: (KnownNat n, 1 <= n) => Int -> FocusedGrid '[Clamped n, Clamped n] Cell -> Int
solve n g = sum [1 | Alive <- toList $ iterate (extend step) g !! n]
partTwo n g = sum [1 | Alive <- toList $ iterate (extend stepCorners) g !! n]

type Rule n = (KnownNat n, 1 <= n) => FocusedGrid '[Clamped n, Clamped n] Cell -> Cell

step :: Rule n
step fg
  | here == Alive && aliveCount `elem` [2, 3] = Alive
  | here == Dead && aliveCount == 3 = Alive
  | otherwise = Dead
  where
    here = extract fg
    aliveCount = sum [1 | Alive <- experiment neighbours fg] :: Int

stepCorners :: Rule n
stepCorners fg
  | isCorner (pos fg) = Alive
  | otherwise = step fg
