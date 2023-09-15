module QuickSort where

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (head' : tail') = quickSort [g | g <- tail', g <= head'] ++ [head'] ++ quickSort [g | g <- tail', g > head']
