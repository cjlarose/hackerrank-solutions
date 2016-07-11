module Main where

import qualified Data.Set as Set
import Control.Monad (replicateM)

readTestCase :: IO (String, String)
readTestCase = do
  l1 <- getLine
  l2 <- getLine
  return (l1, l2)

type StackState = (String, (String, String))

pickStack :: StackState -> Set.Set StackState
pickStack (s, (x:xs, [])) = Set.singleton (x : s, (xs, []))
pickStack (s, ([], y:ys)) = Set.singleton (y : s, ([], ys))
pickStack (s, (x:xs, y:ys))
  | x == y    = Set.fromList [(x : s, (xs, y:ys)), (y : s, (x:xs, ys))]
  | x < y     = Set.singleton (x : s, (xs, y:ys))
  | otherwise = Set.singleton (y : s, (x:xs, ys))

advanceStack :: Set.Set StackState -> Set.Set StackState
advanceStack xs = Set.foldl f (Set.singleton ('[' : (fst . Set.findMin) xs, ("", ""))) xs
  where
    g a m e@(_, (x:xs, []))
      | x < m = pickStack e
      | x == m = Set.union a $ pickStack e
      | otherwise = a
    g a m e@(_, ([], y:xs))
      | y < m = pickStack e
      | y == m = Set.union a $ pickStack e
      | otherwise = a
    g a m e@(_, (x:xs, y:ys))
      | min x y < m = pickStack e
      | min x y == m = Set.union a $ pickStack e
      | otherwise = a
    f a = g a ((head . fst . Set.findMin) $ a)

minimalStack' :: Set.Set StackState -> String
minimalStack' q
  | (== ("", "")) . snd . Set.elemAt 0 $ q = reverse . fst . Set.elemAt 0 $ q
  | otherwise = minimalStack' . advanceStack $ q

minimalStack :: String -> String -> String
minimalStack xs ys = minimalStack' . Set.singleton $ ("", (xs, ys))

main = do
  inputSizeLine <- getLine
  let n = read inputSizeLine
  testCases <- replicateM n readTestCase
  let results = map (\(a, b) -> minimalStack a b) testCases
  mapM putStrLn results
