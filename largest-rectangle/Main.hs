module Main where

readInts :: String -> [Int]
readInts = map read . words

rectangles :: Int -> Int -> [Int] -> Int
rectangles _ maxArea [] = maxArea
rectangles k maxArea heights = rectangles (k + 1) newMax pairwiseMinima where
  pairwiseMinima = zipWith min heights (tail heights)
  newMax = max maxArea (maximum heights * k)

main = do
  _ <- getLine
  heightsLine <- getLine
  let heights = readInts heightsLine
  let maxArea = rectangles 1 (head heights) heights
  putStr $ show maxArea
