{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (findIndex, isPrefixOf, tails)
import Data.Sequence (Seq, index, (|>))
import qualified Data.Sequence as Seq
import Data.Tuple.Extra (fst3)

main :: IO ()
main = do
  input <- map digitToInt <$> readFile "input/2018/14.txt"
  print $ solve input

-- >>> solve [2,0,1,8]
-- >>> solve [5,9,4,1,4]
-- ("5941429882",86764)
-- ("5131221087",2018)
solve :: [Int] -> (String, Int)
solve target = (concatMap show slice, idx)
  where
    n = read $ concatMap show target
    scores = fst3 <$> iterate step (Seq.fromList [3, 7], 0, 1)
    recipes = head [b | b <- scores, Seq.length b >= n + 10]
    slice = Seq.take 10 $ Seq.drop n recipes
    digits = 3 : 7 : [d | (s, s') <- zip scores (tail scores), d <- toList $ Seq.drop (length s) s']
    Just idx = findIndex (isPrefixOf target) (tails digits)

step :: (Seq Int, Int, Int) -> (Seq Int, Int, Int)
step (board, e1, e2) = (board', e1', e2')
  where
    s1 = board `index` e1
    s2 = board `index` e2
    total = s1 + s2
    rs = if total >= 10 then [1, total - 10] else [total]
    board' = foldl (|>) board rs
    len = length board'
    e1' = (e1 + 1 + s1) `mod` len
    e2' = (e2 + 1 + s2) `mod` len
