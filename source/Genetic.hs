-- | this module is aimed at generate genetically programmed ants
module Genetic (I(..)
                , GenAnt
                , geneticAnt
                , generation
                , generationStat
                , generationStatIO
                , generationStatBestIO
                , bestIndividual
                , antGeneticAlgorithm
                , saveGenAnt
                , loadGenAnt
                , savePop
                , loadPop
                , saveGenStat
                ) where

import System.Random
import Data.List
import Helper
import Parameter
import Grid
import Memory
import Game
import Expression
                            
-- | represent two I expressions 
type GenAnt = (I, I)      

-- | crossing-over between i1 and i2
cross :: Parameter -> StdGen -> I -> I -> (I, I)                            
cross param gen i1 i2 = (recSimplifyI i1'', recSimplifyI i2'')
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
    i1'' = if depthI i1' > (popMaxDepth param) then generateI param g (popDepth param) else i1'
    i2'' = if depthI i2' > (popMaxDepth param) then generateI param g' (popDepth param) else i2'
      
-- | crossing-over for GenAnt              
crossAnt :: Parameter -> StdGen -> GenAnt -> GenAnt -> (GenAnt, GenAnt)
crossAnt param gen (i1, i1') (i2, i2') = (cross param g i1 i2, cross param g' i1' i2')
  where
    (g, g') = split gen

-- -- | mutate a I expression      
-- mutate :: Parameter -> StdGen -> I -> I      
-- mutate param gen i = case selectI pos i 1 of
--   B' b -> recSimplifyI $ replaceIB pos i (generateB param g' (nodeB b)) 1
--   I' i' -> recSimplifyI $ replaceII pos i (generateI param g' (nodeI i')) 1
--   where
--     (g, g') = split gen
--     pos = fst (randomR (1, nodeI i) g) :: Int
    
-- | mutate a I expression the terminal node are affected by a small perturbation
mutate :: Parameter -> StdGen -> I -> I      
mutate param gen i = case selectI pos i 1 of
  B' (IsFood r) -> recSimplifyI $ replaceIB pos i (IsFood $ rectMutation g r) 1
  B' (IsEnemy r) -> recSimplifyI $ replaceIB pos i (IsEnemy $ rectMutation g r) 1
  B' b -> recSimplifyI $ replaceIB pos i (generateB param g' (nodeB b)) 1
  I' (NbFood r) -> recSimplifyI $ replaceII pos i (NbFood $ rectMutation g r) 1
  I' (NbEmpty r) -> recSimplifyI $ replaceII pos i (NbEmpty $ rectMutation g r) 1
  I' (NbVisited r) -> recSimplifyI $ replaceII pos i (NbVisited $ rectMutation g r) 1  
  I' (FoodHope r) -> recSimplifyI $ replaceII pos i (FoodHope $ rectMutation g r) 1  
  I' (Const x) -> recSimplifyI $ replaceII pos i (Const (x + fst (randomR (mutateRange param) g))) 1
  I' i' -> recSimplifyI $ replaceII pos i (generateI param g' (nodeI i')) 1
  where
    (g, g') = split gen
    pos = fst (randomR (1, nodeI i) g) :: Int
    rectMutation gen (x, y, dx, dy) =
      (x + fst (randomR (mutateRange param) (g !! 0)), y + fst (randomR (mutateRange param) (g !! 1)), dx, dy)
        where
          g = splits 2 gen


-- | mutation for GenAnt    
mutateAnt :: Parameter -> StdGen -> GenAnt -> GenAnt    
mutateAnt param gen (i, i') = (mutate param g i, mutate param g' i')
  where
    (g, g') = split gen
    
-- | average depth of a population    
averageDepth :: [GenAnt] -> Float
averageDepth pop = fromIntegral (foldl (\ s (i, i') -> s + depthI i + depthI i') 0 pop) / fromIntegral (length pop * 2)

-- | generate a population of genetic programs
generatePop :: Parameter -> StdGen -> Int -> Int -> [GenAnt]    
generatePop param gen n _ = generatePop' param gen n []
  where
    generatePop' param gen n pop
      | n == 0 = pop
      | otherwise = generatePop' param (g !! 2) (pred n) ((recSimplifyI i, recSimplifyI i') : pop)
        where
          g = splits 3 gen
          i = generateI param (g !! 0) (popDepth param) -- expression used to evaluate a N direction movement
          i' = generateI param (g !! 1) (popDepth param) -- expression used to evaluate a NE direction movement

-- | select n individuals from a given population    
selectIndividual :: StdGen -> Int -> [GenAnt] -> [GenAnt]    
selectIndividual gen n pop = selectIndividual' gen n pop []    
  where
    selectIndividual' gen n pop s
      | n == 0 = s
      | otherwise = selectIndividual' g' (pred n) p' (i:s)
        where
          (g, g') = split gen
          (i, p') = removeNth (fst (randomR (0, pred $ length pop) g) :: Int) pop

-- | run a tournament to select an individual
selection :: StdGen -> [GenAnt] -> [GenAnt]
selection gen x = selection' gen x []
  where
    selection' gen x s = case x of
      [] -> s
      [y] -> y:s
      (y:y':xs) -> selection' gen xs ((if winner >= 0.5 then y else y') : s)
        where
          (g, g') = split gen 
          grids = generateGrids g
          winner = matchPercentage $ runMatch grids [geneticAnt y, geneticAnt y'] -- successive programs are coupled and compete to be selected

-- | game statistics - ((x, y), z) x totalScore y totalKill z number game
type Stat = ((Int, Int), Int)             

addStat :: Stat -> Stat -> Stat
addStat ((x, y), z) ((x', y'), z') = ((x + x', y + y'), z + z')
             
-- | tournament to select an individual with game statistics            
selectionStat :: StdGen -> [(Stat, GenAnt)] -> [(Stat, GenAnt)]
selectionStat gen xs = selectionStat' gen xs []
  where
    selectionStat' gen xs ys = case xs of
      [] -> ys
      [x] -> x:ys
      ((s, x):(s', x'):xs') -> selectionStat' gen xs' ((stat, (if winner >= 0.5 then x else x')) : ys)
        where
          (g, g') = split gen
          grids = generateGrids g
          match = runMatch grids [geneticAnt x, geneticAnt x'] -- successive programs are coupled and compete to be selected
          winner = matchPercentage $ match
          stat = s `addStat` s' `addStat` (genAntStat match, nbMatch)
    
-- | best individual of the selection process    
selected :: StdGen -> [GenAnt] -> GenAnt    
selected _ [x] = x
selected gen xs = selected g (selection g' xs)
  where
    (g, g') = split gen
    
-- | best individual of the selection process with game statistics    
selectedStat :: StdGen -> [GenAnt] -> (Stat, GenAnt)    
selectedStat gen xs = selectedStat' gen (zip (repeat ((0, 0), 0)) xs)
  where
    selectedStat' _ [x] = x
    selectedStat' gen xs = selectedStat' g (selectionStat g' xs)
      where
        (g, g') = split gen
    
-- | create 2 new individuals given a population
newIndividual :: Parameter -> StdGen -> [GenAnt] -> (GenAnt, GenAnt)
newIndividual param gen pop = (i1'', i2'')
  where
    g = splits 10 gen
    cro = fst (random $ g !! 0) :: Float
    mut = fst (random $ g !! 1) :: Float
    mut'= fst (random $ g !! 2) :: Float
    i1 = selected (g !! 3) (selectIndividual (g !! 4) (tournamentSize param) pop)
    i2 = selected (g !! 5) (selectIndividual (g !! 6) (tournamentSize param) pop)
    (i1', i2') = if cro < (crossRate param) then crossAnt param (g !! 7) i1 i2 else (i1, i2)
    i1'' = (if mut < (mutateRate param) then mutateAnt param (g !! 8) i1' else i1')
    i2'' = (if mut' < (mutateRate param) then mutateAnt param (g !! 9) i2' else i2')
    
-- | create 2 new individuals given a population with associated game statistics
newIndividualStat :: Parameter -> StdGen -> [GenAnt] -> (Stat, (GenAnt, GenAnt))
newIndividualStat param gen pop = (stat, (i1'', i2''))
  where
    g = splits 10 gen
    cro = fst (random $ g !! 0) :: Float
    mut = fst (random $ g !! 1) :: Float
    mut'= fst (random $ g !! 2) :: Float
    (s1, i1) = selectedStat (g !! 3) (selectIndividual (g !! 4) (tournamentSize param) pop)
    (s2, i2) = selectedStat (g !! 5) (selectIndividual (g !! 6) (tournamentSize param) pop)
    (i1', i2') = if cro < (crossRate param) then crossAnt param (g !! 7) i1 i2 else (i1, i2)
    i1'' = (if mut < (mutateRate param) then mutateAnt param (g !! 8) i1' else i1')
    i2'' = (if mut' < (mutateRate param) then mutateAnt param (g !! 9) i2' else i2')
    stat = s1 `addStat` s2

-- | create a new population using selection and genetic operators
newPop :: Parameter -> StdGen -> [GenAnt] -> [GenAnt]   
newPop param gen pop = newPop' param gen pop (length pop) []          
  where
    newPop' param gen pop n r
      | n <= 1 = r
      | otherwise = newPop' param g' pop (n - 2) (i1:i2:r)
        where
          (g, g') = split gen
          (i1, i2) = newIndividual param g pop
    
-- | create a new population using selection and genetic operators, with statistics
newPopStat :: Parameter -> StdGen -> [GenAnt] -> (Stat, [GenAnt])
newPopStat param gen pop = newPop' param gen pop (length pop) ((0, 0), 0) []          
  where
    newPop' param gen pop n s r
      | n <= 1 = (s, r)
      | otherwise = newPop' param g' pop (n - 2) (s `addStat` s') (i1:i2:r)
        where
          (g, g') = split gen
          (s', (i1, i2)) = newIndividualStat param g pop

-- | generate an evolved population of ant programms
generation :: Parameter -> StdGen -> [GenAnt]
generation param gen = generation' param g' pop (nbGeneration param)
  where
    (g, g') = split gen
    pop = generatePop param g (popSize param) (popDepth param)
    generation' param gen pop n 
      | n == 1 = pop
      | otherwise = (generation' param g (newPop param g' pop) (pred n))
        where
          (g, g') = split gen

-- | generate an evolved population of ant programms, with statistics
generationStat :: Parameter -> StdGen -> ([Stat], [GenAnt])
generationStat param gen = generation' param g' pop (nbGeneration param)
  where
    (g, g') = split gen
    pop = generatePop param g (popSize param) (popDepth param)
    generation' param gen pop n 
      | n == 1 = ([], pop)
      | otherwise = (s:s', pop'')
        where
          (g, g') = split gen
          (s, pop') = newPopStat param g' pop
          (s', pop'') = generation' param g pop' (pred n)
          
generationStatIO :: Parameter -> String -> StdGen -> IO [GenAnt]          
generationStatIO param file gen = generation' param g' pop (nbGeneration param)
  where
    (g, g') = split gen
    pop = generatePop param g (popSize param) (popDepth param)
    generation' param gen pop n
      | n == 1 = return pop
      | otherwise = do
        let (g, g') = split gen
        let (((x, y), z), pop') = newPopStat param g' pop
        let avDepth = averageDepth pop'
        appendFile (file ++ ".dat") ((show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show avDepth) ++ "\n")
        putStr ((show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show avDepth) ++ "\n")            
        savePop (file ++ "_" ++ (show (nbGeneration param - n))) pop
        pop'' <- generation' param g pop' (pred n)
        return pop''
          
generationStatBestIO :: Parameter -> String -> StdGen -> IO [GenAnt]          
generationStatBestIO param file gen = generation' param g' pop (nbGeneration param)
  where
    (g, g') = split gen
    pop = generatePop param g (popSize param) (popDepth param)
    generation' param gen pop n
      | n == 1 = return pop
      | otherwise = do
        let g = splits 3 gen
        let (((x, y), z), pop') = newPopStat param (g !! 0) pop
        let avDepth = averageDepth pop'
        appendFile (file ++ ".dat") ((show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show avDepth) ++ "\n")
        putStr ((show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show avDepth) ++ "\n")
        let ant = bestIndividual (g !! 1) pop
        saveGenAnt (file ++ "_" ++ (show (nbGeneration param - n))) ant
        savePop (file ++ "_" ++ (show (nbGeneration param - n))) pop
        pop'' <- generation' param (g !! 2) pop' (pred n)
        return pop''


-- | select the best individual of a population using a round-robin tournament          
bestIndividual :: StdGen -> [GenAnt] -> GenAnt          
bestIndividual gen pop = robinWinner pop scores
  where
    grids = generateGrids gen
    scores = map matchPercentage (tournament grids selection2 (map (\ x -> geneticAnt x) pop))

-- | perform a complete genetic programming cycle, from population evolution to best individual selection
antGeneticAlgorithm :: Parameter -> StdGen -> GenAnt
antGeneticAlgorithm param gen = bestIndividual g (generation param g')                    
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
saveGenAnt file i = writeFile (file ++ ".ant") (show i)

-- | retrieve a genetic program from the file system
loadGenAnt :: [Char] -> IO GenAnt
loadGenAnt file = do
  x <- readFile (file ++ ".ant")
  return (read x :: GenAnt)
  
-- | save a population to the disk  
savePop :: [Char] -> [GenAnt] -> IO ()  
savePop file pop = writeFile (file ++ ".pop") (show pop)

-- | load a population from the disk
loadPop :: [Char] -> IO [GenAnt]
loadPop file = do
  x <- readFile (file ++ ".pop")
  return (read x :: [GenAnt])

-- | save statistics to the disk
saveGenStat file stats = mapM (\ ((x, y), z) -> appendFile (file ++ ".stat") ((show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ "\n")) (reverse stats)
