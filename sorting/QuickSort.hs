module QuickSort where


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
--version with monads:
quickSort (h:t) = (quickSort (t >>= \x -> if x <= h then [x] else [])) ++ [h] ++ (quickSort (t >>= \x -> if x > h then [x] else []))
--version with list comprehension:
--quickSort (head' : tail') = quickSort [g | g <- tail', g <= head'] ++ [head'] ++ quickSort [g | g <- tail', g > head']
