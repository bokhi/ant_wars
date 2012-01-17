import System.Random
import Grid
import Game
import Ant
import Helper


main = do
  gen <- getStdGen
  let grids = generateGrids gen
  -- mapM (\ x -> putStrLn (show (matchPercentage x))) (tournament grids selection2 (zip (repeat (15, 0)) ruleBasedAnts))
  putStrLn $ show $ matchPercentage $ runMatch grids [((15, 0), user), ((15, 0), superSearcher')] 
  -- replace gready' by predator', hider', wise', precautionary' to play against the other ants
