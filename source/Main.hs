import System.Random
import Grid
import Game
import Ant

main :: IO ()
main = do
  gen <- getStdGen
  let grids = generateGrids gen
  let (winner, games) = runMatch grids [wise, predator]
  putStrLn ("Games : \n" ++ show games ++ "\n winner : " ++ show winner)
  mapM (saveGame "game.txt") games
  saveStat "stat.txt" $ games
