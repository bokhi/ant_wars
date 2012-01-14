import System.Random
import Grid
import Game
import Ant
import Helper
import Genetic
import Parameter

main = do
  gen <- getStdGen
  let (g, g') = split gen
  putStrLn "nbFood nbKill nbGame averageDepth"
  pop <- generationStatIO defaultParameter "../experiment/2.dat" g
  savePop "pop.algo" pop
  -- let i = antGeneticAlgorithm g
  -- saveGenAnt "genAnt.algo" i
  -- let grids = generateGrids g'
  -- mapM (\ x -> putStrLn (show (matchPercentage x))) (tournament grids selection5 ((geneticAnt i) : ruleBasedAnts'))