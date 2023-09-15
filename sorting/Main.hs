module Main where
import Utils
import QuickSort
import MergeSort

main = do 
    list <- randomList 15 (-100) 100
    putStrLn $ "List to order: " ++ show list
    putStrLn $ "Quick sort: " ++ show (quickSort list)
    putStrLn $ "Merge sort: " ++ show (mergeSort list)
