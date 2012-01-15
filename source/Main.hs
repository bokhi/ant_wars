import System.Random
import System.Environment
import Grid
import Game
import Ant
import Helper
import Genetic
import Parameter

main = do
  args <- getArgs
  gen <- getStdGen
  let file = head args
  let param = initParameter $ tail args
  let (g, g') = split gen
  putStrLn "nbFood nbKill nbGame averageDepth"
  pop <- generationStatIO param ("../experiment/" ++ file ++ ".dat") g
  savePop (file ++ ".pop") pop
  -- let i = antGeneticAlgorithm g
  -- saveGenAnt "genAnt.algo" i
  -- let grids = generateGrids g'
  -- mapM (\ x -> putStrLn (show (matchPercentage x))) (tournament grids selection5 ((geneticAnt i) : ruleBasedAnts'))