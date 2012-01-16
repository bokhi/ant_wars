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
                                      
defaultParameter = Parameter { crossRate = 0.5
                             , mutateRate = 0.2
                             , popSize = 750
                             , popDepth = 7
                             , popMaxDepth = 15
                             , tournamentSize = 10
                             , nbGeneration = 50
                             , expressivenessLevel = (9, 9)
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
  
