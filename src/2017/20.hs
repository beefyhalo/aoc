{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Extra (minimumOn, splitOn)
import qualified Data.Map.Strict as M
import Linear.Metric (quadrance)
import Linear.V3 (V3 (..))

data Particle = Particle
  { pid :: !Int,
    pos, vel, acc :: !(V3 Int)
  }

-- $setup
-- >>> input = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>\np=<4,0,0>, v=<0,0,0>, a=<-2,0,0>\np=<4,0,0>, v=<1,0,0>, a=<-1,0,0>\np=<2,0,0>, v=<-2,0,0>, a=<-2,0,0>\np=<4,0,0>, v=<0,0,0>, a=<-1,0,0>\np=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>\np=<3,0,0>, v=<-1,0,0>, a=<-1,0,0>\np=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>"
-- >>> example = zipWith parse [0..] (lines input)

main :: IO ()
main = do
  input <- zipWith parse [0 ..] . lines <$> readFile "input/2017/20.txt"
  print $ solve input
  print $ partTwo input

parse :: Int -> String -> Particle
parse i s = Particle i p v a
  where
    [p, v, a] = parseVec . drop 2 <$> splitOn ", " s
    parseVec l = V3 x y z
      where
        [x, y, z] = read <$> splitOn "," (drop 1 (init l))

-- >>> solve example
-- >>> partTwo example
-- 4
-- 4
solve, partTwo :: [Particle] -> Int
solve = pid . minimumOn key
  where
    key Particle {..} = (quadrance acc, quadrance vel, quadrance pos)
partTwo = length . (!! 1000) . iterate step

step :: [Particle] -> [Particle]
step = removeCollisions . map tick

tick :: Particle -> Particle
tick Particle {..} = Particle pid (pos + v') v' acc
  where
    v' = vel + acc

removeCollisions :: [Particle] -> [Particle]
removeCollisions ps = concat [g | g@[_] <- M.elems groups]
  where
    groups = M.fromListWith (++) [(pos p, [p]) | p <- ps]
