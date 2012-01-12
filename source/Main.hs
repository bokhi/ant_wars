import System.Random
import Grid
import Game
import Ant
import Helper
import Genetic


main = do
  gen <- getStdGen
  let (g, g') = split gen
  let i = antGeneticAlgorithm g
  saveGenAnt "genAnt.algo" i
  let grids = generateGrids g'
  mapM (\ x -> putStrLn (show (matchPercentage x))) (tournament grids selection5 ((geneticAnt i) : ruleBasedAnts'))