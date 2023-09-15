module Main where

import MaxHeap
import System.IO

popPrint :: (Ord nodeType, Show nodeType) => MaxHeap nodeType -> Int -> IO()
popPrint heap iter
  | iter == 0 || len heap == 0 = putStrLn "Empty!"
  | otherwise = do
    let (h, h') = pop heap
    print (h, h')
    popPrint h (iter-1)

main :: IO()
main = do
    putStrLn "Building heap: "
    let heap = buildMaxHeap [10, 1, 23, 32]
    print heap
    putStrLn "Expanding heap: "
    let h = insertList heap [1, 43, 21, 45, 6, 7, 74, 2]
    print h
    putStrLn "Dequeueing: "
    popPrint h (len h)

