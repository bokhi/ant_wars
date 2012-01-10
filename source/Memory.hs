-- | This module contains data and functions aimed at indow ants with memory of their previous actions and movement
module Memory (Memory(..)
               , initMemory
               , updateMemory
               , memoryGrid
               , foodMemory
               ) where

import Data.List
import Helper
import Grid

memoryFood = 15 -- number of fovs a ant can remember related to food
memoryTrack = 15 -- number of previous moves an ant can remember

data Memory = Memory { foods :: [[(Int, Int)]]
                     , tracks :: [(Int, Int)]
                     } deriving Show
                                
-- | initialise the memory of an ant                                
initMemory :: AntNb -> Memory                                
initMemory a = Memory [[]] [antInitialPositions !! a] 
              
-- | store grid information into memory              
updateMemory :: Memory -> AntNb -> Grid -> Memory              
updateMemory m a g = Memory foods'' tracks' 
  where 
    foods' = map (\ xs -> xs \\ fovFilter) (foods m) -- we update the previous states according to the current fov 
    foods'' = take memoryFood (food g):foods' -- time to forget about some previous states
    tracks' = take memoryTrack $ antPosition g a : tracks m
    fovFilter = fovList $ antPosition g a
    
-- | return a list of cases corresponding to a fov
fovList :: (Int, Int) -> [(Int, Int)]
fovList (x, y) = cartProd xs ys
  where
    fov' = fov `div` 2
    tor = map (\ x -> x `mod` dimension) -- remap the positions to the correct interval
    xs = tor [(x - fov')..(x + fov')]
    ys = tor [(y - fov')..(y + fov')]
    
-- | use memory to modify the perception of a grid
memoryGrid :: Memory  -> Grid -> Grid
memoryGrid m g = Grid food' (antPositions g) (score g)
  where
    food' = nub $ concat $ foods m
    
-- | list piece of food that might still be on the grid
foodMemory :: Memory -> [(Int, Int)]    
foodMemory m = nub $ concat $ foods m