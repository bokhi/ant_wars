import System.Random
import Game
import Ant

main :: IO ()
main = do
  gen <- getStdGen
  let (winner, games) = runMatch gen [testMove, predator 1]
  putStrLn ("Games : \n" ++ show games ++ "\n winner : " ++ show winner)
  mapM (saveGame "game.txt") games
  saveStat "stat.txt" $ games
