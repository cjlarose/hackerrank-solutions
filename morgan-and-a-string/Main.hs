module Main where

import qualified Data.Set as Set
import Control.Monad (replicateM)

readTestCase :: IO (String, String)
readTestCase = do
  l1 <- getLine
  l2 <- getLine
  return (l1, l2)

type StackState = (String, (String, String))

pickStack (s, (x:xs, [])) = [(s ++ [x], (xs, []))]
pickStack (s, ([], y:ys)) = [(s ++ [y], ([], ys))]
pickStack (s, (x:xs, y:ys))
  | x == y    = [(s ++ [x], (xs, y:ys)), (s ++ [y], (x:xs, ys))]
  | x < y     = [(s ++ [x], (xs, y:ys))]
  | otherwise = [(s ++ [y], (x:xs, ys))]


minimalStack' :: Set.Set StackState -> String
minimalStack' q
  | (== ("", "")) . snd . Set.elemAt 0 $ q = fst $ Set.elemAt 0 q
  | otherwise = minimalStack' . Set.union newElems . Set.deleteAt 0 $ q
      where
        newElems = Set.fromList . pickStack . Set.elemAt 0 $ q

minimalStack xs ys = minimalStack' . Set.singleton $ ("", (xs, ys))

main = do
  inputSizeLine <- getLine
  let n = read inputSizeLine
  testCases <- replicateM n readTestCase
  let results = map (\(a, b) -> minimalStack a b) testCases
  mapM putStrLn results