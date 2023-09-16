module HeapSort where
import MaxHeap
import Data.Maybe(catMaybes)

heapSort :: (Ord a) => [a] -> [a]
heapSort [] = []
heapSort list = do
    let heap = insertList (buildMaxHeap []) list
    order heap

order :: (Ord a) => MaxHeap a -> [a]
order heap
    | head' == Nothing = []
    | otherwise = (order newHeap) ++ [head $ catMaybes [head']]
    where
        (newHeap, head') = pop heap





