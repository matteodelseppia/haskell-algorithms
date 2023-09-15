module MergeSort where

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = do
    let len = length list
    let center = len `div` 2
    let leftlist = mergeSort (take center list)
    let rightlist = mergeSort [x | (i, x) <- zip [0..] list, i >= center, i < len]
    merge leftlist rightlist

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (lh:lt) (rh:rt)
    | lh < rh = [lh] ++ merge lt (rh:rt)
    | otherwise = [rh] ++ merge (lh:lt) rt
