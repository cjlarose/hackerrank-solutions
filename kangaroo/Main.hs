module Main where

willMeet (x1, v1) (x2, v2) = (dv /= 0) && (dx `mod` dv == 0) && (dx `div` dv >= 0)
  where
    dx = x2 - x1
    dv = v1 - v2

main = do
  inputLine <- getLine
  let (x1:v1:x2:v2:_) = map read . words $ inputLine
  let k1 = (x1, v1)
  let k2 = (x2, v2)
  putStrLn $ if willMeet k1 k2 then "YES" else "NO"
