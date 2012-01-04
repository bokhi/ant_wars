import System.Random
import Game
import Ant

main :: IO ()
main = do
  gen <- getStdGen
  let (winner, games) = runMatch gen [testMove, gready 2]
  putStrLn ("Games : \n" ++ show games ++ "\n winner : " ++ show winner)
  saveGame "game.txt" $ head games
  saveStat "stat.txt" $ games
