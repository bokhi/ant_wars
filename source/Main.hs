import System.Random
import Grid
import Game
import Ant
import Helper


main = do
  gen <- getStdGen
  let grids = generateGrids gen
  putStrLn $ show $ matchPercentage $ runMatch grids [((15, 0), user), ((15, 0), gready')] 
  -- replace gready' by predator', hider', wise', precautionary' to play against the other ants
