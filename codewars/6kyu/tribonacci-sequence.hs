module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) 0 = []
tribonacci (a, b, c) n = a:tribonacci(b, c, a+b+c) (n-1)



tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) 0 = []
tribonacci (a, b, c) n =  take n tribs 
  where tribs = a : b : c : zipWith (+) (zipWith (+) tribs (tail tribs)) (drop 2 tribs)
