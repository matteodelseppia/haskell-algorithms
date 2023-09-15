module Main where
import Utils
import QuickSort
import MergeSort
import InsertionSort

main = do 
    list <- randomList 5 (-100) 100
    putStrLn $ "List to order: " ++ show list
    putStrLn $ "Quick sort: " ++ show (quickSort list)
    putStrLn $ "Merge sort: " ++ show (mergeSort list)
    putStrLn $ "Insertion sort: " ++ show (insertionSort list)
