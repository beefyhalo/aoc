{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (group, sort, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Tree (Tree (Node, rootLabel), unfoldTree)

type Prog = (Int, [String])

main :: IO ()
main = do
  input <- M.fromList . map parse . lines <$> readFile "input/2017/07.txt"
  let root = solve input
  putStrLn $ root
  print $ partTwo root input

parse :: String -> (String, Prog)
parse s = (name, (read weight, drop 1 children))
  where
    name : weight : children = words $ filter (/= ',') s

solve :: M.Map String Prog -> String
solve m = head [n | n <- M.keys m, n `notElem` children]
  where
    children = concatMap snd (M.elems m)

partTwo :: String -> M.Map String (Int, [String]) -> Either Int Int
partTwo root input = findBad tree
  where
    tree = unfoldTree (\name -> let (w, children) = input M.! name in ((name, w), children)) root

findBad :: Tree (String, Int) -> Either Int Int
findBad (Node (_, weight) children) = do
  childWeights <- traverse findBad children

  let grouped = sortOn length $ group $ sort childWeights
  if length grouped <= 1
    then Right (weight + sum childWeights) -- Subtree is balanced
    else
      -- We found the level with the imbalance
      let [wrongWeight : _, rightWeight : _] = grouped
          -- Find the child node that had the wrong weight
          badChildIndex = fromJust $ lookup wrongWeight (zip childWeights [0 ..])
          badChildWeight = snd $ rootLabel (children !! badChildIndex)
       in Left (badChildWeight + rightWeight - wrongWeight)
