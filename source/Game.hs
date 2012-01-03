-- | This module contains functions to run matches between different implementations of ant behaviours
module Game (Game(..)
            , initGame
            , runGame
            , runMatch
            , saveGame
            , tournament
            ) where

import System.Random
import Data.List
import Helper
import Grid
import Ant

nbMove = 35
nbMatch = 5
nbVictory = 3

data Game = Game {initialGrid :: Grid
                 , grid :: Grid
                 , ants :: [Ant]
                 } 
                            
instance Show Game where
  show g = show (initialGrid g) ++ "\n" ++ show (ants g) ++ "\n"

-- | Initialise a game
initGame :: Grid -> [Grid -> Direction] -> Game
initGame gr mvs = Game gr gr a
  where a = map (\ (mv, i) -> initAnt i mv) $ zip mvs [1..]

-- | Given a game and a specific ant, update the grid by moving the ant
updateGame :: Game -> AntNb -> Game
updateGame g a = 
  if kill $ ants g !! (a `mod` antNumber) -- if the ant has been killed no update - works only for two ants
  then g
  else Game (initialGrid g) gr' (replaceNth (pred a) ant' (ants g))
    where
      ant = ants g !! (pred a `mod` antNumber)
      gr = grid g
      (ant', gr') = updateAnt ant gr
    

      
-- | To test whether a game is over or not      
endGame :: Game -> Bool      
endGame g = any (\ a -> length (directions a) >= nbMove) (ants g)
            
-- | Winner of a game
winnerGame :: Game -> AntNb    
winnerGame g = antNb (head $ sortBy (\ a a' -> compare (score a') (score a)) (ants g)) -- TODO verify that 1 is winning in case of tie --> seems to be OK

-- | run a game between two ants     
runGame :: Game -> Game
runGame g = runGame' g 0
  where
    runGame' g i = if endGame g 
                   then g
                   else runGame' (updateGame g (succ i)) (succ i `mod` length (ants g))

-- | run a set of games
runMatch :: StdGen -> [Grid -> Direction] -> (AntNb, [Game])
runMatch gen moves = (if score 1 >= 3 then 1 else 2, games') -- Works only for two ants
  where
    grids = take nbMatch (generateGrids gen)
    games = map (\ grid -> initGame grid moves) grids 
    games' = map runGame games
    winners = map winnerGame games'
    score ant = foldl (\ sum nb -> if nb == ant then sum + 1 else sum) 0 winners

-- | tournament between mutiple ants
tournament = undefined

-- | save a game to the filesystem
saveGame :: Grid -> IO ()
saveGame = undefined
