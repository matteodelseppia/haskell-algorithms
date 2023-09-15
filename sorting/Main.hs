module Main where
import Utils
import InsertionSort

main = do 
    list <- randomList 15 (-100) 100
    putStrLn $ "List to order: " ++ show list
    putStrLn $ "Insertion sort: " ++ show (insertionSort list)