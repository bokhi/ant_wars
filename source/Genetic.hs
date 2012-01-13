-- | this module is aimed at generate genetically programmed ants
module Genetic (I(..)
                , GenAnt
                , geneticAnt
                , antGeneticAlgorithm
                , saveGenAnt
                , loadGenAnt
                ) where

import System.Random
import Data.List
import Helper
import Grid
import Memory
import Game
import Expression

crossRate = 0.8 --crossing-over mutation rate
mutateRate = 0.1 -- mutation rate
popSize = 10 -- size of the program population
popDepth = 6 -- initial maximum depth of newly created individuals
tournamentSize = 5 -- size of the tournament caracterising the selection pressure
nbGeneration = 3 -- number of generation the algorithm is run

                            
-- | represent two I expressions 
type GenAnt = (I, I)      

-- | crossing-over between i1 and i2
cross :: StdGen -> I -> I -> (I, I)                            
cross gen i1 i2 = (i1', i2')
  where
    (g, g') = split gen
    pos1 = fst (randomR (1, nodeI i1) g) :: Int
    pos2 = fst (randomR (1, nodeI i2) g') :: Int
    i1' = case (selectI pos2 i2 1, selectI pos1 i1 1) of 
      (B' b, B' _) -> replaceIB pos1 i1 b 1 
      (I' i, I' _) -> replaceII pos1 i1 i 1
      _ -> i1 -- the branchs' type are incompatible, no modification is made
    i2' = case (selectI pos1 i1 1, selectI pos2 i2 1) of
      (B' b, B' _) -> replaceIB pos2 i2 b 1
      (I' i, I' _) -> replaceII pos2 i2 i 1
      _ -> i2 -- the branchs' type are incompatible, no modification is made
      
-- | crossing-over for GenAnt              
crossAnt :: StdGen -> GenAnt -> GenAnt -> (GenAnt, GenAnt)
crossAnt gen (i1, i1') (i2, i2') = (cross g i1 i2, cross g' i1' i2')
  where
    (g, g') = split gen

-- | mutate a I expression      
mutate :: StdGen -> I -> I      
mutate gen i = case selectI pos i 1 of
  B' b -> replaceIB pos i (generateB g' (nodeB b)) 1
  I' i' -> replaceII pos i (generateI g' (nodeI i')) 1
  where
    (g, g') = split gen
    pos = fst (randomR (1, nodeI i) g) :: Int
    
-- | mutation for GenAnt    
mutateAnt :: StdGen -> GenAnt -> GenAnt    
mutateAnt gen (i, i') = (mutate g i, mutate g' i')
  where
    (g, g') = split gen
    
-- | generate a population of genetic programs
generatePop :: StdGen -> Int -> Int -> [GenAnt]    
generatePop _ 0 _ = []
generatePop gen n d = (i, i') : generatePop g'' (pred n) d
  where
    (g, gen') = split gen
    (g', g'') = split gen'
    i = generateI g d -- expression used to evaluate a N direction movement
    i' = generateI g' d -- expression used to evaluate a NE direction movement
    
-- | select n individuals from a given population    
selectIndividual :: StdGen -> Int -> [GenAnt] -> [GenAnt]    
selectIndividual _ 0 _ = []
selectIndividual gen n p = i : selectIndividual g' (pred n) p'
  where 
    (g, g') = split gen
    (i, p') = removeNth (fst (randomR (0, pred $ length p) g) :: Int) p

-- | run a tournament to select an individual
selection :: StdGen -> [GenAnt] -> [GenAnt]
selection gen [] = []
selection gen [x] = [x]
selection gen (x:x':xs) = (if winner >= 0.5 then x else x') : selection g' xs 
  where 
    (g, g') = split gen 
    grids = generateGrids g
    winner = matchPercentage $ runMatch grids [geneticAnt x, geneticAnt x'] -- successive programs are coupled are compete to be selected
    
-- | best individual of the selection process    
selected :: StdGen -> [GenAnt] -> GenAnt    
selected _ [x] = x
selected gen xs = selected g (selection g' xs)
  where
    (g, g') = split gen
    
-- | create 2 new individuals given a population
newIndividual :: StdGen -> [GenAnt] -> [GenAnt]
newIndividual gen pop = [i1'', i2'']
  where
    g = splits 10 gen
    cro = fst (random $ g !! 0) :: Float
    mut = fst (random $ g !! 1) :: Float
    mut'= fst (random $ g !! 2) :: Float
    i1 = selected (g !! 3) (selectIndividual (g !! 4) tournamentSize pop)
    i2 = selected (g !! 5) (selectIndividual (g !! 6) tournamentSize pop)
    (i1', i2') = if cro < crossRate then crossAnt (g !! 7) i1 i2 else (i1, i2)
    i1'' = (if mut < mutateRate then mutateAnt (g !! 8) i1' else i1')
    i2'' = (if mut' < mutateRate then mutateAnt (g !! 8) i2' else i2')
    
-- | create a new population using selection and genetic operators
newPop :: StdGen -> [GenAnt] -> [GenAnt]   
newPop gen pop = newPop' gen pop $ length pop
  where
    newPop' gen pop n 
      | n <= 1 = []
      | otherwise = newIndividual g pop ++ newPop' g' pop (n - 2)
        where
          (g , g') = split gen
    
-- | generate an evolved population of ant programms
generation :: StdGen -> [GenAnt]
generation gen = generation' g' pop nbGeneration
  where
    (g, g') = split gen
    pop = generatePop g popSize popDepth
    generation' gen pop n 
      | n == 1 = pop
      | otherwise = generation' g (newPop g' pop) (pred n)
        where
          (g, g') = split gen
          
-- | select the best individual of a population using a round-robin tournament          
bestIndividual :: StdGen -> [GenAnt] -> GenAnt          
bestIndividual gen pop = robinWinner pop scores
  where
    grids = generateGrids gen
    scores = map matchPercentage (tournament grids selection2 (map (\ x -> geneticAnt x) pop))

-- | perform a complete genetic programming cycle, from population evolution to best individual selection
antGeneticAlgorithm :: StdGen -> GenAnt
antGeneticAlgorithm gen = bestIndividual g (generation g')                    
  where
    (g , g') = split gen
    
-- | The I expression trees are used to evaluate the result of N and NE movement on the grid 
-- rotation properties of the problem are thus exploited
geneticAnt :: GenAnt -> AntNb -> Memory -> Grid -> Direction 
geneticAnt (t, t') a m g = d
  where
    g' = rotateGrid g
    g'' = rotateGrid g'
    g''' = rotateGrid g''
    gs = [g, g', g'', g''']
    ds = [N, W, S, E, NE, NW, SW, SE]
    es = map (evalI t a m) gs
    es' = map (evalI t' a m) gs
    (_, d) = maximumBy (\ x x' -> compare (fst x) (fst x')) (zip (es ++ es') ds) -- the greatest number gives us the direction to go to 

-- | save a genetic program to the file system
saveGenAnt :: [Char] -> GenAnt -> IO ()    
saveGenAnt file i = writeFile file (show i)

-- | retrieve a genetic program from the file system
loadGenAnt :: [Char] -> IO GenAnt
loadGenAnt file = do
  x <- readFile file
  return (read x :: GenAnt)




