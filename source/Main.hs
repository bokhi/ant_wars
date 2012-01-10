import System.Random
import Grid
import Game
import Ant
import Helper


main = do
  gen <- getStdGen
  let grids = generateGrids gen
  mapM (\ x -> putStrLn (show (matchPercentage x))) (tournament grids selection2 ruleBasedAnts')
