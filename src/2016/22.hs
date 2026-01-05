{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Node = Node {x, y, size, used, avail :: Int} deriving (Show, Eq)

-- $setup
-- >>> input = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . drop 2 . lines <$> readFile "input/2016/22.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Node
parse line = Node x y size used avail
  where
    name : ints = words line
    [_, x, y] = read . drop 1 <$> splitOn "-" name
    [size, used, avail, _] = map (read . init) ints

-- >>> solve example
-- >>> partTwo example
-- 7
-- 7
solve, partTwo :: [Node] -> Int
solve ns = length [(a, b) | a <- ns, b <- ns, a /= b, used a > 0, used a <= avail b]
partTwo ns = fromMaybe 0 distToGoal + 5 * (maxX - 1) + 1 -- Each move toward the goal uses 5 steps + the final swap.
  where
    maxX = maximum $ map x ns
    maxY = maximum $ map y ns

    emptyNode = head [n | n <- ns, used n == 0]
    startPos = (x emptyNode, y emptyNode)
    goal = (maxX - 1, 0)
    walls = Set.fromList [(x n, y n) | n <- ns, used n > size emptyNode]
    distToGoal = bfs startPos goal walls maxX maxY

bfs :: (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Int -> Int -> Maybe Int
bfs start goal walls mx my = go (Seq.singleton (start, 0)) (Set.singleton start)
  where
    go Empty _ = Nothing
    go ((curr, dist) :<| rest) seen
      | curr == goal = Just dist
      | otherwise = go rest' seen'
      where
        next = [p | p <- neighbors curr, Set.notMember p seen, Set.notMember p walls, inBounds p]
        seen' = foldr Set.insert seen next
        rest' = rest <> Seq.fromList [(p, dist + 1) | p <- next]
        neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        inBounds (x, y) = x >= 0 && x <= mx && y >= 0 && y <= my
