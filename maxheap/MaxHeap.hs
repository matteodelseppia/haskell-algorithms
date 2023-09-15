module MaxHeap where
import Data.List

data MaxHeap a = MaxHeap {  
  array :: [a],
  len :: Int
} deriving (Show)

buildMaxHeap :: (Ord a) => [a] -> MaxHeap a
buildMaxHeap [] = MaxHeap {array = [], len = 0}
buildMaxHeap arr = MaxHeap {array = (reverse.sort)arr, len = (length arr)}

heapMax :: (Ord a) => MaxHeap a -> Maybe a
heapMax heap
    | null (array heap) = Nothing
    | otherwise = Just ((head.array) heap)

slice :: (Ord a) => [a] -> Int -> Int -> [a]
slice array from to = case from >= to of
    True -> []
    False -> take (to - from) (drop from array)

swap :: (Ord a) => [a] -> Int -> Int -> Int -> [a]
swap array first second size = do 
    let firstValue = array !! first
    let secondValue = array !! second
    ((take first array) ++ [secondValue] ++ (slice array (first+1) second) ++ [firstValue] ++ (slice array (second+1) size))

heapifyAfterInsert :: (Ord a) => [a] -> Int -> Int -> [a]
heapifyAfterInsert heap 0 _ = heap
heapifyAfterInsert heap pos size
    | fatherValue >= value = heap
    | otherwise = do
        let newHeap = (swap heap fatherPos pos size)
        heapifyAfterInsert newHeap fatherPos size
    where
        value = heap !! pos
        fatherPos = (pos-1) `div` 2
        fatherValue = heap !! fatherPos

insertOne :: (Ord a) => MaxHeap a -> a -> MaxHeap a
insertOne heap element = do
    let arr = array heap
    let size = len heap
    let newArr = heapifyAfterInsert (arr ++ [element]) size (size+1)
    heap { array = newArr, len = size + 1}

insertList :: (Ord a) => MaxHeap a -> [a] -> MaxHeap a
insertList heap [] = heap
insertList heap (h:t) = insertList (insertOne heap h) t

pop :: (Ord a) => MaxHeap a -> (MaxHeap a, Maybe a)
pop heap 
    | null (array heap) = (heap, Nothing)
    | otherwise = do
            let out = heapMax heap
            let arr = array heap
            let oldLen = len heap
            let (_:tail') = arr
            let newArr = [last tail'] ++ (init tail')
            let adjusted = heapifyAfterPop newArr 0 (oldLen - 1)
            (heap { array = adjusted, len = (oldLen - 1)}, out)


heapifyAfterPop :: (Ord a) => [a] -> Int -> Int -> [a]
heapifyAfterPop array pos 0 = []
heapifyAfterPop array pos size
    | leftSonIndex >= size = array
    | leftSonIndex < size || rightSonIndex < size = do
        let leftSonVal = array !! leftSonIndex
        let rightSonVal =  if rightSonIndex < size then (array !! rightSonIndex) else val
        if leftSonVal > val || rightSonVal > val then do
            let (toSwapIndex, toSwapVal) = if leftSonVal > rightSonVal then (leftSonIndex, (array !! leftSonIndex))  else (rightSonIndex, (array !! rightSonIndex))
            let newHeap = (swap array pos toSwapIndex size)
            heapifyAfterPop newHeap toSwapIndex size
        else
            array

    where leftSonIndex = (2*pos + 1) 
          rightSonIndex = (2*pos + 2)
          val = array !! pos
