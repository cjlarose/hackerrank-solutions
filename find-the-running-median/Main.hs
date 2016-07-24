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
  | size maxHeap <  size minHeap = fromIntegral $ peekMax minHeap

-- case 1: heaps of equal size, peekMax maxHeap <= x <= peekMax minHeap
-- ([5.0], [8.0]) insert 7.0
-- Insert into maxHeap on left
-- ([7.0, 5.0], [8.0])
-- Or insert into minHeap on right
-- ([5.0], [7.0, 8.0])
--
-- case 1.5: heaps of equal size, peekMax maxHeap <= peekMax minHeap < x
-- ([2.0], [4.0]) insert 7.0
-- Inserting into the maxHeap would violate the constraint, so insert into minHeap
-- ([2.0], [4.0, 7.0])
--
-- case 1.75 heaps of equal size, x < peekMax maxHeap <= peekMax minHeap
-- ([5.0], [8.0]) insert 2.0
-- Inserting into minHeap would violate constraint, so insert into maxHeap
-- ([5.0, 2.0], [8.0])
--
--
-- case 2: heaps of unequal size
-- Insert into heap according to case 1
-- ([7.0, 5.0], [12.0]) insert 14.0
-- ([7.0, 5.0], [12.0, 14.0])
--
-- ([5.0], [7.0, 12.0]) insert 14.0
-- ([5.0], [7.0, 12.0, 14.0])
-- Then, rebalance
-- ([7.0, 5.0], [12.0, 14.0])
--
-- ([7.0, 5.0], [12.0]) insert 2.0
-- ([7.0, 5.0, 2.0], [12.0])
-- Then, rebalance
-- ([5.0, 2.0], [7.0, 12.0])
rebalance (maxHeap, minHeap)
  | size maxHeap - size minHeap >= 2 = (popMax maxHeap, insert (peekMax maxHeap) minHeap)
  | size minHeap - size maxHeap >= 2 = (insert (peekMax minHeap) maxHeap, popMax minHeap)
  | otherwise = (maxHeap, minHeap)

insertHeapPair x (maxHeap, minHeap)
  | size minHeap == 0 = rebalance (insert x maxHeap, minHeap)
  | x > peekMax minHeap = rebalance (maxHeap, insert x minHeap)
  | otherwise = rebalance (insert x maxHeap, minHeap)

f (ms, heapPair) x = (median newHeapPair : ms, newHeapPair)
  where
    newHeapPair = insertHeapPair x heapPair

runningMedians :: Int -> [Int] -> [Float]
runningMedians n (x:xs) = fst $ foldl f ([fromIntegral x], (insert x $ empty compare n, empty (flip compare) n)) xs
