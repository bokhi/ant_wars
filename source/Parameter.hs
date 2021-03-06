-- | This module contains a data type containing all the genetic algorithm parameters
module Parameter (Parameter(..)
                 , defaultParameter
                 , initParameter
                 ) where
       
data Parameter = Parameter { crossRate :: Float --crossing-over mutation rate
                           , mutateRate :: Float -- mutation rate
                           , mutateRange :: (Int, Int) -- interval of perturbation of a terminal node value
                           , popSize :: Int -- size of the program population
                           , popDepth :: Int -- initial maximum depth of newly created individuals
                           , popMaxDepth :: Int -- maximum depth accepted within a population
                           , tournamentSize :: Int -- size of the tournament caracterising the selection pressure
                           , nbGeneration :: Int -- number of generation the algorithm is run
                           , expressivenessLevel ::  (Int, Int) -- express which part of the grammar are used to construct B and I expressions - ranging from (0, 0) to (6, 10)
                           } deriving (Show, Read)
                                      
defaultParameter = Parameter { crossRate = 0.5
                             , mutateRate = 0.2
                             , mutateRange = (-1, 1)
                             , popSize = 500
                             , popDepth = 7
                             , popMaxDepth = 15
                             , tournamentSize = 10
                             , nbGeneration = 100
                             , expressivenessLevel = (12, 12)
                             }
initParameter [] = defaultParameter
initParameter (cr:mu:ps:pd:pmd:ts:nb:eb:ei:[]) = Parameter { crossRate = read cr :: Float                              
                                                        , mutateRate = read mu :: Float
                                                        , mutateRange = (-1, 1)
                                                        , popSize = read ps :: Int
                                                        , popDepth = read pd :: Int
                                                        , popMaxDepth = read pmd :: Int
                                                        , tournamentSize = read ts :: Int
                                                        , nbGeneration = read nb :: Int
                                                        , expressivenessLevel = (read eb, read ei) :: (Int, Int)
                                                        }
  
