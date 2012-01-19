import System.Random
import System.Environment
import Grid
import Game
import Ant
import Helper
import Genetic
import Parameter

main = 
  do
    args <- getArgs
    gen <- getStdGen
    let action = head args
    let file = head (drop 1 args)
    let param = initParameter $ drop 2 args
    case action of
      "pop" -> -- store every step of the pop evolution  
        do
          putStrLn "nbFood nbKill nbGame averageDepth"
          pop <- generationStatIO param ("../experiment/" ++ file) gen
          savePop (file ++ ".pop") pop
      "ant" -> -- read a pop file and find its best ant
        do
          pop <- loadPop ("../experiment/" ++ file)
          let ind = bestIndividual gen pop
          saveGenAnt ("../experiment/" ++ file) ind
      "gen" -> -- combine the two previous cases
        do
          let (g, g') = split gen
          putStrLn "nbFood nbKill nbGame averageDepth"
          pop <- generationStatBestIO param ("../experiment/" ++ file) g
          savePop (file ++ ".pop") pop
          let ind = bestIndividual g' pop
          saveGenAnt file ind
      "tour" -> -- tournament between the specified evolved ant and the rule based antsa
        do
          ant <- loadGenAnt $ head args
          let (a:b:c:d:e:[]) = map matchPercentage $ tournament (generateGrids gen) selection5 ((geneticAnt ant) : ruleBasedAnts')
          appendFile ("../experiment/standardBestAnt.dat") ((show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ "\n")
      "play" -> -- play a game against the specified evolved ant
        do
          ant <- loadGenAnt $ head $ drop 1 args
          let grids = generateGrids gen
          let game = initGame (head grids) (user:[geneticAnt ant])
          let game' = runGame game
          putStrLn ("ant " ++ (show $ gameWinner game') ++ "wins")
      _ -> 
        putStrLn "See documentation, or code source :)"



