{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  input <- B.readFile "input/2016/16.txt"
  B.putStrLn $ solve 272 input
  B.putStrLn $ solve 35651584 input

solve :: Int -> B.ByteString -> B.ByteString
solve n = checksum . B.take n . dragon n

dragon :: Int -> B.ByteString -> B.ByteString
dragon n = until ((>= n) . B.length) dragonStep
  where
    invert = B.map (\c -> if c == '0' then '1' else '0')
    dragonStep a = a <> "0" <> B.reverse (invert a)

-- >>> checksum "110010110100"
-- "100"
checksum :: B.ByteString -> B.ByteString
checksum = until (odd . B.length) (B.pack . pairCheck . B.unpack)
  where
    pairCheck (a : b : xs) = (if a == b then '1' else '0') : pairCheck xs
    pairCheck _ = ""
