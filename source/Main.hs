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
          pop <- loadPop (file ++ ".pop")
          let ind = bestIndividual gen pop
          saveGenAnt file ind
      _ -> -- combine the two previous cases
        do
          let (g, g') = split gen
          putStrLn "nbFood nbKill nbGame averageDepth"
          pop <- generationStatBestIO param ("../experiment/" ++ file) g
          savePop (file ++ ".pop") pop
          let ind = bestIndividual g' pop
          saveGenAnt file ind