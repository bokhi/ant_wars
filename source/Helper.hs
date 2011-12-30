-- | This module contains helper functions needed by other modules
module Helper (replaceNth) where
       
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

