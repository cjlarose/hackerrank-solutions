module Main where

import Control.Monad (replicateM, (<=<))
import Data.List (permutations)

readTestCase :: IO (String, String)
readTestCase = do
  l1 <- getLine
  l2 <- getLine
  return (l1, l2)

pickStack (s, (x:xs, [])) = [(x : s, (xs, []))]
pickStack (s, ([], y:ys)) = [(y : s, ([], ys))]
pickStack (s, (x:xs, y:ys))
  | x == y    = [(x : s, (xs, y:ys)), (y : s, (x:xs, ys))]
  | x < y     = [(x : s, (xs, y:ys))]
  | otherwise = [(y : s, (x:xs, ys))]

inSteps n = foldr (<=<) return (replicate n pickStack)

minimalStack xs ys = minimum . map (reverse . fst) $ allResults
  where
    n = length xs + length ys
    allResults = inSteps n ("", (xs, ys))

main = do
  inputSizeLine <- getLine
  let n = read inputSizeLine
  testCases <- replicateM n readTestCase
  let results = map (\(a, b) -> minimalStack a b) testCases
  mapM putStrLn results
