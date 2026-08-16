{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List.Extra (minimumOn, wordsBy)
import Data.Set qualified as S

data Star = Star {x, y, vx, vy :: Int}

-- $setup
-- >>> input = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2018/10.txt"
  let (pic, t) = solve input
  putStrLn pic
  print t

parse :: String -> Star
parse s = Star x y vx vy
  where
    [x, y, vx, vy] = read <$> wordsBy (\c -> not $ c == '-' || isDigit c) s

-- >>> solve example
-- ("#...#..###\n#...#...#.\n#...#...#.\n#####...#.\n#...#...#.\n#...#...#.\n#...#...#.\n#...#..###\n",3)
solve :: [Star] -> (String, Int)
solve input = (render pts, time)
  where
    (pts, time) = minimumOn (getHeight . fst) frames
    frames = zip (iterate (map step) input) [0 .. 20000]

step :: Star -> Star
step s@Star {..} = s {x = x + vx, y = y + vy}

getHeight :: [Star] -> Int
getHeight stars = maximum ys - minimum ys
  where
    ys = map y stars

render :: [Star] -> String
render stars =
  unlines
    [ [if (c, r) `S.member` pts then '#' else '.' | c <- [minimum xs .. maximum xs]]
    | r <- [minimum ys .. maximum ys]
    ]
  where
    pts = S.fromList [(x s, y s) | s <- stars]
    (xs, ys) = unzip (S.toList pts)
