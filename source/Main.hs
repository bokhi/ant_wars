import System.Random
import Game
import Ant

main :: IO ()
main = do
  gen <- getStdGen
  let (winner, games) = runMatch gen [testMove, testMove]
  putStrLn (show games)
