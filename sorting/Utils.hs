module Utils where

import Data.List
import System.Random

randomList :: Int -> Int -> Int -> IO [Int]
randomList len min' max' = sequence $ replicate len (randomRIO(min', max'))