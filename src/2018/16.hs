{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Bits ((.&.), (.|.))
import Data.Char (isDigit)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List (find)
import Data.List.Split (chunksOf, splitOn, wordsBy)

type Regs = [Int]

type Instr = (Int, Int, Int, Int)

ops :: [Regs -> Int -> Int -> Int -> Regs]
ops =
  [ bin (+) True True,
    bin (+) True False,
    bin (*) True True,
    bin (*) True False,
    bin (.&.) True True,
    bin (.&.) True False,
    bin (.|.) True True,
    bin (.|.) True False,
    \r a _ c -> set r c (r !! a),
    \r a _ c -> set r c a,
    cmp (>) False True,
    cmp (>) True False,
    cmp (>) True True,
    cmp (==) False True,
    cmp (==) True False,
    cmp (==) True True
  ]
  where
    set r i v = let (left, _ : right) = splitAt i r in left ++ v : right
    bin f useA useB r a b c = set r c (f va vb)
      where
        va = if useA then r !! a else a
        vb = if useB then r !! b else b
    cmp f = bin (\x y -> if f x y then 1 else 0)

-- $setup
-- >>> input = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n\n\n\n9 2 1 2"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2018/16.txt"
  print $ solve input

parse :: String -> ([(Regs, Instr, Regs)], [Instr])
parse input = (parseSample <$> chunksOf 4 samplesBlock, parseInstr <$> programLines)
  where
    [samplesBlock, programLines] = lines <$> splitOn "\n\n\n\n" input
    parseSample (a : b : c : _) = (parseRegs a, parseInstr b, parseRegs c)
    parseInstr l = let [a, b, c, d] = map read (words l) in (a, b, c, d)
    parseRegs = map read . wordsBy (not . isDigit)

-- >>> fst $ solve example
-- 1
solve :: ([(Regs, Instr, Regs)], [Instr]) -> (Int, Int)
solve (samples, program) = (overThree, head finalRegs)
  where
    overThree = length $ filter ((>= 3) . S.size . matchingOps) samples
    poss = buildPoss samples
    mapping = resolve poss
    finalRegs = runProg mapping program

matchingOps :: (Regs, Instr, Regs) -> IntSet
matchingOps (before, (_, a, b, c), after) = S.fromList [i | (i, f) <- zip [0 ..] ops, f before a b c == after]

-- deduce mapping
buildPoss :: [(Regs, Instr, Regs)] -> IM.IntMap IntSet
buildPoss samples = IM.fromListWith (<>) [(op, matchingOps s) | s@(_, (op, _, _, _), _) <- samples]

resolve :: IM.IntMap IntSet -> IM.IntMap Int
resolve = go IM.empty
  where
    go solved poss
      | IM.null poss = solved
      | otherwise = go (IM.insert op val solved) poss'
      where
        Just (op, s) = find ((== 1) . S.size . snd) (IM.toList poss)
        val = S.findMin s
        poss' = S.delete val <$> IM.delete op poss

runProg :: IM.IntMap Int -> [Instr] -> Regs
runProg mapping = foldl' step [0, 0, 0, 0]
  where
    step regs (op, a, b, c) =
      let f = ops !! (mapping IM.! op) in f regs a b c
