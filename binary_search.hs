import System.IO
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Data.List

parseSort :: [String] -> [Int] -> [Int]
parseSort [] out = sort out
parseSort (head':tail') out = case readMaybe head'::Maybe Int of
  Just x -> parseSort tail' (out ++ [x])
  Nothing -> parseSort tail' out

binarySearch :: (Ord a) => [a] -> a -> Int -> Int -> Int
binarySearch [] _ _ _ = -1
binarySearch numbers num start end = case start > end of
  True -> -1
  False -> do 
    let midIndex = (start + end) `div` 2
    let midValue = numbers!!midIndex
    ret midIndex midValue num where
      ret midIndex midValue num
         | midValue < num = binarySearch numbers num (midIndex+1) end
         | midValue > num = binarySearch numbers num start (midIndex-1)
         | midValue == num = midIndex


main :: IO()
main = do 
  putStrLn "Please enter a list of integers"
  inputString <- getLine
  let numbers = parseSort (words inputString) []
  putStrLn "Please enter an integer to search"
  print numbers
  numString <- getLine
  let numTemp = parseSort (words numString) []
  case length numTemp == 1 of
    True -> do
      let [num] = numTemp
      putStrLn "The number is at index (N.B.: -1 if absent):"
      print (binarySearch numbers num 0 ((length numbers) - 1))
    False -> putStrLn "Invalid integer!"
  main