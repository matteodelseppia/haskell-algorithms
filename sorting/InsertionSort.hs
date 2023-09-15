module InsertionSort where

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (head' : tail') = insertionSort [g | g <- tail', g <= head'] ++ [head'] ++ insertionSort [g | g <- tail', g > head']
