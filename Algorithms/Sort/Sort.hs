module Sort
(mergeSort
,insertionSort
)
where

mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge ( mergeSort left) (mergeSort right)
                where left = take ((length xs) `div` 2) xs
                      right = drop ((length xs) `div` 2) xs
merge:: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
            | x < y = x:(merge xs (y:ys))
            | otherwise = y:(merge (x:xs) ys)


insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = foldr insert [] (x:xs)

insert:: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
            | x < y = x:(y:ys)
            | otherwise = y:(insert x ys)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort [m | m <- xs, m <= x] ++ [x] ++ quickSort [n | n <- xs, n >= x] 