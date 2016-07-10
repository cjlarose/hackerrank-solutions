module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, fromMaybe)

listMin :: [Int] -> Maybe Int
listMin xs
  | null xs = Nothing
  | otherwise = Just . minimum $ xs

minDistance :: [Int] -> Maybe Int
minDistance positions
  | length positions < 2 = Nothing
  | otherwise = Just . minimum $ zipWith (flip (-)) positions (tail positions)

minimumDistance :: [Int] -> Maybe Int
minimumDistance xs = listMin . map fromJust . filter isJust $ distances
  where
    positions = Map.elems . Map.fromListWith Set.union $ zip xs (map Set.singleton [0..])
    distances = map (minDistance . Set.toAscList) positions

main = do
  n <- getLine
  xsLine <- getLine
  let xs = map read . words $ xsLine
  let rs = (fromMaybe (-1)) . minimumDistance $ xs
  print rs
