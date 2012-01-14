-- | This module contains a data type containing all the genetic algorithm parameters
module Parameter (Parameter(..)
                 , defaultParameter
                 , initParameter
                 ) where
       
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
initParameter [] = defaultParameter
initParameter (cr:mu:ps:pd:pmd:ts:nb:eb:ei:[]) = Parameter { crossRate = read cr :: Float                              
                                                        , mutateRate = read mu :: Float
                                                        , popSize = read ps :: Int
                                                        , popDepth = read pd :: Int
                                                        , popMaxDepth = read pmd :: Int
                                                        , tournamentSize = read ts :: Int
                                                        , nbGeneration = read nb :: Int
                                                        , expressivenessLevel = (read eb, read ei) :: (Int, Int)
                                                        }
  
