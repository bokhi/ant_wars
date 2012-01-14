-- | This module contains a data type containing all the genetic algorithm parameters
module Parameter (Parameter(..)
                 , defaultParameter) where
       
data Parameter = Parameter { crossRate :: Float --crossing-over mutation rate
                           , mutateRate :: Float -- mutation rate
                           , popSize :: Int -- size of the program population
                           , popDepth :: Int -- initial maximum depth of newly created individuals
                           , popMaxDepth :: Int
                           , tournamentSize :: Int -- size of the tournament caracterising the selection pressure
                           , nbGeneration :: Int -- number of generation the algorithm is run
                           , expressivenessLevel ::  (Int, Int) -- express which part of the grammar are used to construct B and I expressions - ranging from (0, 0) to (6, 10)
                           } deriving (Show, Read)
                                      
defaultParameter = Parameter { crossRate = 0.7
                          , mutateRate = 0.1
                          , popSize = 50
                          , popDepth = 5
                          , popMaxDepth = 10
                          , tournamentSize = 3
                          , nbGeneration = 50
                          , expressivenessLevel = (5, 4)
                          }

                             
  
