-- | This module contains helper functions needed by other modules
module Helper (replaceNth
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
       
replaceNth n newVal [] = [newVal]
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
                
                
couple :: a -> [a] -> [[a]]                
couple x xs = map (\ x' -> [x, x']) xs

selection1 xs = [[x, x] | x <- xs]

selection2 xs = [[x, y] | x <- xs, y <- xs]

selection3 [x] = [[x, x]]
selection3 xs = couple (head xs) xs ++ selection3 (tail xs)

selection4 [x] = []
selection4 (x:xs) = (couple x xs) ++ (selection4 xs)

selection5 (x:xs) = couple x xs
  
              
group' :: Int -> [a] -> [[a]]
group' n xs = xs':group' n xs''
  where
    (xs', xs'') = splitAt n xs
    
cartProd xs ys = [(x, y) | x <- xs, y <- ys]    
