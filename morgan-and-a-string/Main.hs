module Main where

import Control.Monad (replicateM)
import Data.List (minimumBy)

readTestCase :: IO (String, String)
readTestCase = do
  l1 <- getLine
  l2 <- getLine
  return (l1, l2)

type StackState = (String, (String, String))

pickStack (s, (x:xs, [])) = [(x : s, (xs, []))]
pickStack (s, ([], y:ys)) = [(y : s, ([], ys))]
pickStack (s, (x:xs, y:ys))
  | x == y    = [(x : s, (xs, y:ys)), (y : s, (x:xs, ys))]
  | x < y     = [(x : s, (xs, y:ys))]
  | otherwise = [(y : s, (x:xs, ys))]

pruneStep xs = filter (((==) minEl) . fst) xs
  where minEl = minimum . map fst $ xs

minimalStack' :: [StackState] -> String
minimalStack' q
  | (== ("", "")) . snd . head $ q = reverse . fst . head $ q
  | otherwise = minimalStack' $ pruneStep $ {-# SCC "concatMap" #-} concatMap pickStack $ q

minimalStack :: String -> String -> String
minimalStack xs ys = minimalStack' [("", (xs, ys))]

main = do
  inputSizeLine <- getLine
  let n = read inputSizeLine
  testCases <- replicateM n readTestCase
  let results = map (\(a, b) -> minimalStack a b) testCases
  mapM putStrLn results
