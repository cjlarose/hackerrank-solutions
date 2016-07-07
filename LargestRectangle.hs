module LargestRetangle where

readInts :: String -> [Int]
readInts = (map read) . words

rectangles :: Int -> [Int] -> [Int]
rectangles _ [] = []
rectangles k heights = (maximum heights * k) : (rectangles (k + 1) pairwiseMinima) where
  pairwiseMinima = zipWith min heights (tail heights)

main = do
  _ <- getLine
  heightsLine <- getLine
  let heights = readInts heightsLine
  let maxArea = (maximum . (rectangles 1)) $ heights
  putStr $ show maxArea
