module Main where

import Data.List (partition, foldl')

readInts :: String -> [Int]
readInts = map read . words

partition' p = foldl' (select p) ([],[])
  where
    select p (t, f) x
      | p x = (x : t, f)
      | otherwise = (t, x : f)

factors n = [d | d <- [2..n], n `mod` d == 0]

primes = [p | p <- [2..], null . tail . factors $ p]

stackPlates q plates = fmt $ foldl f ([], reverse plates) (take q primes)
  where
    fmt (stacked, remaining) = (concat . reverse) stacked ++ remaining
    f (stacked, plates) p = (newStacked : stacked, remaining)
      where
        (newStacked, remaining) = partition' (\x -> x `mod` p == 0) plates

main = do
  inputSizeLine <- getLine
  let (n:q:[]) = readInts inputSizeLine
  platesLine <- getLine
  let plates = readInts platesLine
  mapM print $ stackPlates q plates
