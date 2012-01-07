-- | This module contains data and functions aimed at indow ants with memory of their previous actions and movement
module Memory (Memory(..)
               , updateMemory
               , memoryGrid
               ) where

import Grid


memoryFood = 10 -- number of fovs a ant can remember related to food
memoryTrack = 10 -- number of previous moves an ant can remember

data Memory = Memory { foods :: [[(Int, Int)]]
                       tracks :: [(Int, Int)]
                     }
              
-- | store grid information into memory              
updateMemory :: Memory -> AntNb -> Grid -> Memory              
updateMemory m a g = undefined

-- | use memory to modify the perception of a grid
memoryGrid :: Memory -> Grid -> Grid
updateMemory m g = undefined