{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Crypto.Hash (MD5, hash)
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B

type Pos = (Int, Int)

type Path = B.ByteString

-- $setup
-- >>> example = "ihgpwlah"

main :: IO ()
main = do
  input <- B.readFile "input/2016/17.txt"
  B.putStrLn $ solve input
  print $ partTwo input

-- >>> solve example
-- >>> partTwo example
-- "DDRRRD"
-- 370
solve :: B.ByteString -> Path
solve pass =
  head [path | ((3, 3), path) <- concat $ iterate (concatMap (neighbors pass)) [((0, 0), "")]]

partTwo :: B.ByteString -> Int
partTwo pass = dfs ((0, 0), "")
  where
    dfs ((3, 3), path) = B.length path
    dfs state = maximum (0 : map dfs (neighbors pass state))

neighbors :: B.ByteString -> (Pos, Path) -> [(Pos, Path)]
neighbors pass ((x, y), path) =
  [ (pos, path <> B.singleton d)
  | ((d, pos), True) <- zip moves (openDoors $ pass <> path),
    x >= 0 && x <= 3 && y >= 0 && y <= 3
  ]
  where
    moves = [('U', (x, y - 1)), ('D', (x, y + 1)), ('L', (x - 1, y)), ('R', (x + 1, y))]

openDoors :: B.ByteString -> [Bool]
openDoors bs = take 4 $ (`B.elem` "bcdef") <$> B.unpack hex
  where
    hex = BA.convertToBase BA.Base16 (hash @_ @MD5 bs)
