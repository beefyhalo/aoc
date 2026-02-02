{-# LANGUAGE TupleSections #-}

import Data.List (partition)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

-- >>> partTwo 2 15 example
-- 21
partTwo :: Int -> Int -> Graph -> Int
partTwo limit base = step 0 []
  where
    cost n = base + fromEnum n - fromEnum 'A' + 1

    step t busy graph
      | null graph && null busy = t
      -- Can we put someone to work?
      | length busy < limit && not (null available) = step t ((n, t + cost n) : busy) graph
      -- No one else can start, so we must advance time.
      | otherwise = step t' working graph'
      where
        available = [n | (n, deps) <- M.toList graph, S.null deps, n `notElem` map fst busy]
        n = minimum available
        t' = minimum (map snd busy)
        (done, working) = partition ((== t') . snd) busy
        doneNodes = S.fromList (map fst done)

        graph' = (S.\\ doneNodes) <$> foldr M.delete graph doneNodes
