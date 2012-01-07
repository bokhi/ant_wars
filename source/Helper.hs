-- | This module contains helper functions needed by other modules
module Helper (replaceNth
              , couple
              , pair
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

pair :: [a] -> [[a]]
pair [x] = []
pair (x:xs) = (couple x xs) ++ (pair xs)
  
              
group' :: Int -> [a] -> [[a]]
group' n xs = xs':group' n xs''
  where
    (xs', xs'') = splitAt n xs
    
cartProd xs ys = [(x, y) | x <- xs, y <- ys]    
