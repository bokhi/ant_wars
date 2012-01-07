-- | This module contains data and functions aimed at indow ants with memory of their previous actions and movement
module Memory (Memory(..)
               , initMemory
               , updateMemory
               , memoryGrid
               ) where

import Data.List
import Helper
import Grid


memoryFood = 10 -- number of fovs a ant can remember related to food
memoryTrack = 10 -- number of previous moves an ant can remember

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
    foods'' = take memoryFood foods' -- time to forget about some previous states
    tracks' = take memoryTrack $ antPosition g a : tracks m
    fovFilter = fovList $ antPosition g a

    
-- | return a list of cases corresponding to a fov
fovList :: (Int, Int) -> [(Int, Int)]
fovList (x, y) = cartProd xs ys
  where
    fov' = fov `div` 2
    tor = map (\ x -> x `mod` dimension)
    xs = tor [(x - fov')..(x + fov')]
    ys = tor [(y - fov')..(y + fov')]
    
-- | use memory to modify the perception of a grid
memoryGrid :: Memory -> AntNb -> Grid -> Grid
memoryGrid m g = undefined