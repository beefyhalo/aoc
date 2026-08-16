import Data.Map.Strict qualified as M
import Data.Set qualified as S

type Node = Char

type Graph = M.Map Node (S.Set Node)

-- $setup
-- >>> input = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/2018/07.txt"
  putStrLn $ solve input
  print $ partTwo 5 60 input

parse :: [String] -> Graph
parse xs = M.fromListWith (<>) edges <> initGraph
  where
    edges = [(s !! 36, S.singleton (s !! 5)) | s <- xs]
    nodes = concatMap (\s -> [s !! 5, s !! 36]) xs
    initGraph = M.fromList [(n, S.empty) | n <- nodes]

-- >>> solve example
-- "CABDFE"
solve :: Graph -> [Node]
solve g
  | null g = []
  | otherwise = n : solve g'
  where
    (n, _) = M.findMin (M.filter null g)
    g' = S.delete n <$> M.delete n g

-- >>> partTwo 2 0 example
-- 15
partTwo :: Int -> Int -> Graph -> Int
partTwo limit base = step 0 M.empty
  where
    cost n = base + fromEnum n - fromEnum 'A' + 1

    step t busy g
      | null g && null busy = t
      -- Fill worker slots
      | length busy < limit && not (null ready) = step t (M.insert n (t + cost n) busy) g
      -- Advance time
      | otherwise = step t' working g'
      where
        ready = M.filter null (M.difference g busy)
        (n, _) = M.findMin ready
        t' = minimum busy
        (done, working) = M.partition (== t') busy
        g' = (S.\\ M.keysSet done) <$> M.difference g done
