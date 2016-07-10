module Main where

import Data.List (partition)

readInts :: String -> [Int]
readInts = map read . words

primeFactors n = [d | d <- [2..n], n `mod` d == 0]

primes = [p | p <- [2..], null . tail . primeFactors $ p]

stackPlates q plates = fmt $ foldl f ([], reverse plates) (take q primes)
  where
    fmt (stacked, remaining) = (concat . reverse) stacked ++ remaining
    f (stacked, plates) p = (reverse newStacked : stacked, reverse remaining)
      where
        (newStacked, remaining) = partition (\x -> x `mod` p == 0) plates

main = do
  inputSizeLine <- getLine
  let (n:q:[]) = readInts inputSizeLine
  platesLine <- getLine
  let plates = readInts platesLine
  mapM print $ stackPlates q plates
