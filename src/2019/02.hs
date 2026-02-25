{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Arrow ((&&&))
import Control.Monad.ST (ST, runST)
import Data.List.Split (wordsBy)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Memory = V.Vector Int

-- $setup
-- >>> input = "1,9,10,3,2,3,11,0,99,30,40,50,0"
-- >>> example = parse input

main :: IO ()
main = print . (solve &&& partTwo) . parse =<< readFile "input/2019/02.txt"

parse :: String -> Memory
parse = V.fromList . map read . wordsBy (== ',')

-- >>> solve example
-- 100
solve :: Memory -> Int
solve mem = runST $ do
  m <- V.thaw mem
  MV.write m 1 12
  MV.write m 2 2
  run m
  MV.read m 0

partTwo :: Memory -> Int
partTwo mem =
  head
    [ 100 * noun + verb
    | noun <- [0 .. 99],
      verb <- [0 .. 99],
      let res = runST $ do
            m <- V.thaw mem
            MV.write m 1 noun
            MV.write m 2 verb
            run m
            MV.read m 0,
      res == 19690720
    ]

run :: MV.MVector s Int -> ST s ()
run m = go 0
  where
    go ip = do
      opcode <- MV.read m ip
      case opcode of
        1 -> step (+)
        2 -> step (*)
        _ -> pure ()
      where
        step f = do
          aPos <- MV.read m (ip + 1)
          bPos <- MV.read m (ip + 2)
          dst <- MV.read m (ip + 3)
          a <- MV.read m aPos
          b <- MV.read m bPos
          MV.write m dst (f a b)
          go (ip + 4)
