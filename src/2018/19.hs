{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits ((.&.), (.|.))
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as V

type Regs = Vector Int

data Instr = Instr String Int Int Int

exec :: Instr -> Regs -> Regs
exec (Instr op a b c) regs = regs // [(c, val)]
  where
    val = case op of
      "addr" -> regs ! a + regs ! b
      "addi" -> regs ! a + b
      "mulr" -> regs ! a * regs ! b
      "muli" -> regs ! a * b
      "banr" -> regs ! a .&. regs ! b
      "bani" -> regs ! a .&. b
      "borr" -> regs ! a .|. regs ! b
      "bori" -> regs ! a .|. b
      "setr" -> regs ! a
      "seti" -> a
      "gtir" -> if a > regs ! b then 1 else 0
      "gtri" -> if regs ! a > b then 1 else 0
      "gtrr" -> if regs ! a > regs ! b then 1 else 0
      "eqir" -> if a == regs ! b then 1 else 0
      "eqri" -> if regs ! a == b then 1 else 0
      "eqrr" -> if regs ! a == regs ! b then 1 else 0

-- $setup
-- >>> input = "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/2018/19.txt"
  print $ solve input

parse :: [String] -> (Int, Vector Instr)
parse (ipLine : progLines) = (read ipReg, V.fromList prog)
  where
    ipReg = last $ words ipLine
    prog = map parseInstr progLines

    parseInstr line = Instr op a b c
      where
        (op : args) = words line
        [a, b, c] = map read args

-- >>> solve example
-- (6,0)
solve :: (Int, Vector Instr) -> (Int, Int)
solve (ipReg, prog) = (sumDivs part1, sumDivs part2)
  where
    regs0 = V.replicate 6 0
    part1 = findTarget ipReg prog regs0
    part2 = findTarget ipReg prog (regs0 // [(0, 1)])

    sumDivs n = sum [i | i <- [1 .. n], n `mod` i == 0]

-- findTarget: run until the program is about to enter the divisor loop (ip == 1)
findTarget :: Int -> V.Vector Instr -> Regs -> Int
findTarget ipReg prog = (! 1) . until (\r -> r ! ipReg == 1) (step ipReg prog)

step :: Int -> V.Vector Instr -> Vector Int -> Vector Int
step ipReg prog r = r' // [(ipReg, ip')]
  where
    ip = r ! ipReg
    r' = exec (prog ! ip) (r // [(ipReg, ip)])
    ip' = r' ! ipReg + 1
