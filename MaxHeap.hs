module MaxHeap where
import Data.List
import Text.Read
import Debug.Trace

data MaxHeap nodeType = MaxHeap {  
  array :: [nodeType],
  len :: Int
} deriving (Show)

buildMaxHeap :: (Num nodeType, Ord nodeType) => [nodeType] -> MaxHeap nodeType
buildMaxHeap [] = MaxHeap {array = [], len = 0}
buildMaxHeap arr = MaxHeap {array = (reverse.sort)arr, len = (length arr)}

heapMax :: (Num nodeType, Ord nodeType) => MaxHeap nodeType -> Maybe nodeType
heapMax heap = case null (array heap) of
    True -> Nothing
    False -> Just ((head.array) heap)

slice :: (Num a) => [a] -> Int -> Int -> [a]
slice array from to = case from >= to of
    True -> []
    False -> take (to - from) (drop from array)

swap :: (Num nodeType, Ord nodeType) => [nodeType] -> Int -> Int -> Int -> [nodeType]
swap array first second size = do 
    let firstValue = array !! first
    let secondValue = array !! second
    ((take first array) ++ [secondValue] ++ (slice array (first+1) second) ++ [firstValue] ++ (slice array (second+1) size))

heapifyAfterInsert :: (Num nodeType, Ord nodeType) => [nodeType] -> Int -> Int -> [nodeType]
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

insertOne :: (Num nodeType, Ord nodeType) => MaxHeap nodeType -> nodeType -> MaxHeap nodeType
insertOne heap element = do
    let arr = array heap
    let size = len heap
    let newArr = heapifyAfterInsert (arr ++ [element]) size (size+1)
    heap { array = newArr, len = size + 1}

insertList :: (Num nodeType, Ord nodeType) => MaxHeap nodeType -> [nodeType] -> MaxHeap nodeType
insertList heap [] = heap
insertList heap (h:t) = insertList (insertOne heap h) t

pop :: (Num nodeType, Ord nodeType) => MaxHeap nodeType -> (MaxHeap nodeType, Maybe nodeType)
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


heapifyAfterPop :: (Num nodeType, Ord nodeType) => [nodeType] -> Int -> Int -> [nodeType]
heapifyAfterPop array pos 0 = []
heapifyAfterPop array pos size
    | leftSonIndex >= size = trace "leftSonIndex >= size" array
    | leftSonIndex < size || rightSonIndex < size = do
        let leftSonVal = array !! leftSonIndex
        let rightSonVal =  if rightSonIndex < size then (array !! rightSonIndex) else (val - 1)
        if leftSonVal > val || rightSonVal > val then do
            let (toSwapIndex, toSwapVal) = if leftSonVal > rightSonVal then (leftSonIndex, (array !! leftSonIndex))  else (rightSonIndex, (array !! rightSonIndex))
            let newHeap = (swap array pos toSwapIndex size)
            heapifyAfterPop newHeap toSwapIndex size
        else
            array

    where leftSonIndex = (2*pos + 1) 
          rightSonIndex = (2*pos + 2)
          val = array !! pos
