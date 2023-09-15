module InsertionSort where

import Data.List

adjust :: (Ord a) => [a] -> Int -> [a]
adjust [] _ = []
adjust [x] _ = [x]
adjust list 0 = list
adjust list pos 
    | previous < current = list
    | otherwise = adjust ((take (pos-1) list) ++ [current] ++ [previous] ++ (drop (pos + 1) list)) (pos-1)
    where
        current = list !! pos
        previous = list !! (pos-1)
        len = length list

insertionSort' :: (Ord a) => [a] -> Int -> [a]
insertionSort' [] _ = []
insertionSort' [x] _ = [x]
insertionSort' list pos
    | pos >= len = list
    | current > previous = insertionSort' list (pos+1)
    | otherwise = insertionSort' (adjust list pos) (pos+1)
    where
        current = list !! pos
        previous = list !! (pos-1)
        len = length list

insertionSort :: (Ord a) => [a] -> [a]
insertionSort list = insertionSort' list 1