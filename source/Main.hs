import System.Random
import Game
import Ant

main :: IO ()
main = do
  gen <- getStdGen
  let (winner, games) = runMatch gen [testMove, testMove]
  putStrLn ("Games : \n" ++ show games ++ "\n winner : " ++ show winner)
