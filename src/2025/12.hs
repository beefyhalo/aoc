{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isSpace)
import Data.List.Split (chunksOf)

-- $setup
-- >>> input = "0:\n###\n##.\n##.\n\n1:\n###\n##.\n.##\n\n2:\n.##\n###\n##.\n\n3:\n##.\n###\n##.\n\n4:\n###\n#..\n###\n\n5:\n###\n.#.\n###\n\n4x4: 0 0 0 0 2 0\n12x5: 1 0 1 0 2 2\n12x5: 1 0 1 0 3 2"
-- >>> (exampleShapes, exampleRegions) = parse (lines input)

type Shape = [String]

type Region = (Int, Int, [Int])

main :: IO ()
main = do
  (shapes, regions) <- parse . lines <$> readFile "input/2025/12.txt"
  print $ solve shapes regions

parse :: [String] -> ([Shape], [Region])
parse ls = (shapes, regions)
  where
    clean = filter (not . all isSpace) ls
    shapeBlocks = take 6 $ chunksOf 4 clean
    shapes = map (drop 1) shapeBlocks
    regions = map parseRegion (drop (6 * 4) clean)

    parseRegion s = (read w, read h, map read $ words rest)
      where
        (dim, ':' : rest) = break (== ':') s
        (w, 'x' : h) = break (== 'x') dim

-- >>> solve exampleShapes exampleRegions
-- 3
solve :: [Shape] -> [Region] -> Int
solve shapes = length . filter (solveRegion shapes)

-- >>> solveRegion exampleShapes (12,5,[1,0,1,0,3,2])
-- True
solveRegion :: [Shape] -> Region -> Bool
solveRegion shapes (w, h, counts) = total <= w * h
  where
    total = sum $ zipWith (*) areas counts
    areas = map (sum . map (length . filter (== '#'))) shapes
