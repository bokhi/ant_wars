import System.Random
import Grid
import Game
import Ant
import Helper


main = do
  gen <- getStdGen
  let grids = generateGrids gen
  --  let games = runMatch grids [hider, hider]
  --  putStrLn $ show $ matchPercentage games
--  putStrLn $ show $ matchPercentage $ head (tournament grids selection1 ruleBasedAnts)
  mapM (\ x -> putStrLn (show (matchPercentage x))) (tournament grids selection2 ruleBasedAnts)
  --  putStrLn ("Games : \n" ++ show games ++ "\n winner : " ++ show winner)
  -- mapM (saveGame "game.txt") games
  -- saveStat "stat.txt" $ games
  -- let tour = snd $ unzip $ tournament grids [testMove, gready, predator, hider, wise]
  -- putStrLn $ concat $ map matchStat tour
