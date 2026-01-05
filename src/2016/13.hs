import Data.Bits (popCount)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Point = (Int, Int)

main :: IO ()
main = do
  input <- read <$> readFile "input/2016/13.txt"
  print $ solve input
  print $ partTwo input

solve, partTwo :: Int -> Int
solve = fromMaybe 0 . fst . bfs (== (31, 39)) 1000 (1, 1)
partTwo = Set.size . snd . bfs (const False) 50 (1, 1)

isOpen :: Int -> Point -> Bool
isOpen n (x, y) = even $ popCount $ x * x + 3 * x + 2 * x * y + y + y * y + n

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

bfs :: (Point -> Bool) -> Int -> Point -> Int -> (Maybe Int, Set.Set Point)
bfs stop maxDist start n = go (Seq.singleton (start, 0)) (Set.singleton start)
  where
    go Seq.Empty seen = (Nothing, seen)
    go ((here, dist) Seq.:<| rest) seen
      | stop here = (Just dist, seen)
      | dist >= maxDist = go rest seen
      | otherwise = go rest' seen'
      where
        next = [p | p <- neighbors here, isOpen n p, Set.notMember p seen, p >= (0, 0)]
        rest' = rest <> Seq.fromList [(p, dist + 1) | p <- next]
        seen' = foldr Set.insert seen next
