module Main where

import Data.Array.IArray (Array, array, bounds, (!), (//), elems)

type Heap = (Int -> Int -> Ordering, Array Int Int, Int)

heapToList (_, xs, sz) = take sz $ elems xs

isEmpty :: Heap -> Bool
isEmpty (_, _, sz) = sz == 0

empty :: (Int -> Int -> Ordering) -> Int -> Heap
empty f capacity = (f, array (0, pred capacity) [], 0)

size (_, _, sz) = sz

peekMax :: Heap -> Int
peekMax h@(_, xs, _)
  | isEmpty h = error "Empty heap"
  | otherwise = xs ! 0

parent idx = (pred idx) `div` 2
leftChild idx = 2 * idx + 1
rightChild idx = 2 * idx + 2

bubbleUp :: Int -> Heap -> Heap
bubbleUp 0 h = h
bubbleUp idx h@(cmp, xs, sz)
  | (xs ! idx) `cmp` (xs ! (parent idx)) == LT = h
  | otherwise = bubbleUp (parent idx) (cmp, newHeap, sz)
                  where
                    newHeap = xs // [(idx, xs ! (parent idx)), (parent idx, xs ! idx)]

insert :: Int -> Heap -> Heap
insert x (cmp, xs, size) = bubbleUp size (cmp, xs // [(size, x)], succ size)

bubbleDown :: Int -> Heap -> Heap
bubbleDown idx h@(cmp, xs, size)
  | leftChild idx >= size && rightChild idx >= size = h
  | leftChild idx < size && (rightChild idx >= size || ((xs ! leftChild idx) `cmp` (xs ! rightChild idx) == GT)) && (xs ! idx) `cmp` (xs ! leftChild idx) == LT = bubbleDown (leftChild idx) (cmp, xs // [(idx, xs ! leftChild idx), (leftChild idx, xs ! idx)], size)
  | (xs ! idx) `cmp` (xs ! rightChild idx) == LT = bubbleDown (rightChild idx) (cmp, xs // [(idx, xs ! rightChild idx), (rightChild idx, xs ! idx)], size)
  | otherwise = h

popMax :: Heap -> Heap
popMax h@(cmp, xs, sz)
  | isEmpty h = error "Empty heap"
  | otherwise = bubbleDown 0 (cmp, xs // [(0, xs ! pred sz)], pred sz)

type HeapPair = (Heap, Heap)

median :: HeapPair -> Float
median (maxHeap, minHeap)
  | size maxHeap == size minHeap = fromIntegral (peekMax maxHeap + peekMax minHeap) / 2
  | size maxHeap >  size minHeap = fromIntegral $ peekMax maxHeap

insertHeapPair x (maxHeap, minHeap)
  | size maxHeap == size minHeap = (insert x maxHeap, minHeap)
  | size maxHeap >  size minHeap = (maxHeap, insert x minHeap)

runningMedians :: Int -> [Int] -> [Float]
runningMedians n (x:xs) = fst $ foldl f ([], (insert x $ empty compare n, empty (flip compare) n)) xs
  where
    f (ms, heapPair) x = (median heapPair : ms, insertHeapPair x heapPair)
