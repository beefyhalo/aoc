{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Lens
import Control.Monad (foldM)
import Control.Monad.State.Strict (State, evalState)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (..))

data Node = Clean | Weakened | Infected | Flagged deriving (Eq, Enum)

data Dir = U | R | D | L deriving (Eq, Enum, Bounded)

type Coord = (Int, Int)

type BurstRule = Node -> (Dir -> Dir, Node, Int)

data VirusState = VirusState
  { _grid :: M.Map Coord Node,
    _pos :: Coord,
    _dir :: Dir
  }

makeLenses ''VirusState

-- $setup
-- >>> input = "..#\n#..\n..."
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2017/22.txt"
  print $ solve 10000 rules1 input
  print $ solve 10000000 rules2 input

parse :: String -> VirusState
parse input = VirusState initialMap (0, 0) U
  where
    initialLines = lines input
    h = length initialLines
    w = length (head initialLines)
    initialMap =
      M.fromList
        [ ((x - w `div` 2, y - h `div` 2), Infected)
        | (y, row) <- zip [0 ..] initialLines,
          (x, '#') <- zip [0 ..] row
        ]

-- >>> solve 10000 rules1 example
-- >>> solve 10000000 rules2 example
-- 5587
-- 2511944
solve :: Int -> BurstRule -> VirusState -> Int
solve n rules = getSum . evalState (foldMapM' (\_ -> burst rules) [1 .. n])

rules1, rules2 :: BurstRule
rules1 = \case
  Clean -> (turnLeft, Infected, 1)
  _ -> (turnRight, Clean, 0)
rules2 = \case
  Clean -> (turnLeft, Weakened, 0)
  Weakened -> (id, Infected, 1)
  Infected -> (turnRight, Flagged, 0)
  Flagged -> (reverseDir, Clean, 0)

burst :: BurstRule -> State VirusState (Sum Int)
burst rules = do
  p <- use pos
  cur <- use (grid . at p . non Clean)
  let (dFunc, newNode, inc) = rules cur
  grid . at p .= if newNode == Clean then Nothing else Just newNode
  newD <- uses dir dFunc
  dir .= newD
  pos %= stepCoord newD
  pure $ Sum inc

turnLeft, turnRight, reverseDir :: Dir -> Dir
turnLeft d = toEnum $ (fromEnum d + 3) `mod` 4
turnRight d = toEnum $ (fromEnum d + 1) `mod` 4
reverseDir d = toEnum $ (fromEnum d + 2) `mod` 4

stepCoord :: Dir -> Coord -> Coord
stepCoord d (x, y) = case d of
  U -> (x, y - 1)
  D -> (x, y + 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

foldMapM' :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM' f = foldM (\acc x -> f x >>= \r -> pure $! acc <> r) mempty
