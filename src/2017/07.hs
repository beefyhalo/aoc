{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Either (fromLeft)
import Data.List (elemIndex, group, sort, sortOn)
import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as M
import Data.Tree (Tree (Node, rootLabel), unfoldTree)

type Prog = (Int, [String])

main :: IO ()
main = do
  input <- M.fromList . map parse . lines <$> readFile "input/2017/07.txt"
  let root = solve input
  putStrLn root
  print $ partTwo root input

parse :: String -> (String, Prog)
parse s = (name, (read weight, drop 1 children))
  where
    name : weight : children = wordsBy (`elem` ", ") s

solve :: M.Map String Prog -> String
solve m = head [n | n <- M.keys m, n `notElem` children]
  where
    children = concatMap snd (M.elems m)

partTwo :: String -> M.Map String Prog -> Int
partTwo root input = fromLeft 0 $ findBad tree
  where
    tree = unfoldTree (\name -> let (w, children) = input M.! name in ((name, w), children)) root

findBad :: Tree (String, Int) -> Either Int Int
findBad (Node (_, w) children) = do
  childWeights <- traverse findBad children
  case sortOn length (group $ sort childWeights) of
    [[wrongWeight], rightWeight : _] -> Left (badWeight + diff)
      where
        diff = rightWeight - wrongWeight
        Just badIndex = elemIndex wrongWeight childWeights
        (_, badWeight) = rootLabel (children !! badIndex)
    _ -> Right (w + sum childWeights) -- Subtree is balanced