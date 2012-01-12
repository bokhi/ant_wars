-- | This module contains helper functions needed by other modules
-- These functions are processing lists, and mostly used to generate
-- different tournaments given a list of competitors
module Helper (replaceNth
              , robin
              , robinWinner
              , splits
              , removeNth
              , couple
              , selection1
              , selection2
              , selection3
              , selection4
              , selection5
              , group'
              , cartProd
              ) where

import Data.List
import System.Random

-- | give the result of list condisered as a round-robin tournament
robin :: Int -> Int -> [a] -> [b] -> b
robin x y l l' = l' !! ((pred x) * (length l) - ((x * (pred x)) `div` 2) + y - x - 1)

-- | winner of a round-robin tournament
robinWinner l l' =  l !! (maximumBy (\ x y -> compare (score x) (score y)) [1..(length l)] - 1)
  where
    score x = sum (map (\ y -> if x < y then robin x y l l' else 1 - robin y x l l') [x' | x' <- [1..(length l)], x /= x'])

-- | construct mutiple generators from a single seed
splits :: Int -> StdGen -> [StdGen]
splits 1 gen = [gen]
splits n gen = g : splits (pred n) g'
  where 
    (g, g') = split gen

-- | remove the n-nth element of a list
removeNth :: Int -> [a] -> (a, [a])
removeNth pos l = removeNth' pos l [] 0
removeNth' pos l l' n  
  | pos == n = (head l, reverse l' ++ tail l)
  | otherwise = removeNth' pos (tail l) (head l : l') (succ n)

-- | replace the n-nth element of a list
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal [] = [newVal] -- if the index is too big, we add the element at the rear
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
                
-- | pair a value with every single element of a list                 
couple :: a -> [a] -> [[a]]                
couple x xs = [[x, x'] | x' <- xs]

-- | pair every single element of a list with itself
selection1 :: [a] -> [[a]]
selection1 xs = [[x, x] | x <- xs]

-- | pair elements from a list without taking care of ordering 
selection2 :: [a] -> [[a]]
selection2 xs = [[x, y] | x <- xs, y <- xs]

-- | pair elements from a list taking care of ordering
selection3 :: [a] -> [[a]]
selection3 [x] = [[x, x]]
selection3 xs = couple (head xs) xs ++ selection3 (tail xs)

-- | pair elements from a list taking care of ordering and not with itself
selection4 :: [a] -> [[a]]
selection4 [x] = []
selection4 (x:xs) = (couple x xs) ++ (selection4 xs)

-- | pair the first element of the list the the tail
selection5 :: [a] -> [[a]]
selection5 (x:xs) = couple x xs
                
-- | group the element of a list by packet of size n
group' :: Int -> [a] -> [[a]]
group' n xs = xs':group' n xs''
  where
    (xs', xs'') = splitAt n xs
    
-- | cartesian product of two lists    
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]    
