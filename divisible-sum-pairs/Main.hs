module Main where

pairs [] = []
pairs (x:xs) = zip (repeat x) xs ++ pairs xs

divisibleSumPairs k = length . filter (\x -> x `mod` k == 0) . map sumPair . pairs
  where
    sumPair (a, b) = a + b

main = do
  sizeLine <- getLine
  let (n:k:_) = map read . words $ sizeLine
  inputLine <- getLine
  let xs = map read . words $ inputLine
  print . divisibleSumPairs k $ xs
