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

-- | Initialise a game
initGame :: Grid -> [Grid -> Direction] -> Game
initGame gr mvs = Game gr gr a
  where a = map (\ (mv, i) -> initAnt i mv) $ zip mvs [1..]

-- | Given a game and a specific ant, update the grid by moving the ant
updateGame :: Game -> AntNb -> Game
updateGame g a = Game (initialGrid g) gr' (replaceNth (pred a) ant' (ants g))
  where
    updateAnt a g = 
      let direction = move a $ g
          g' = updateGrid g (antNb a) direction  
      in (Ant (antNb a) (direction:(directions a)) (score a + (foodLeft g - foodLeft g')) (antCollision g') (move a), g')
    ant = ants g !! (pred a)
    gr = grid g
    (ant', gr') = updateAnt ant gr
      
-- | To test whether a game is over or not      
endGame :: Game -> Bool      
endGame g = dead || end
  where
    dead = any (\ a -> kill a)  $ ants g
    end = all (\ a -> length (directions a) >= nbMove) (ants g)
            
-- | run a game between two ants     
runGame :: Game -> Game
runGame g = runGame' g 0
  where
    runGame' g i = if endGame g 
                   then g 
                   else runGame' (updateGame g (succ i)) (succ i `mod` length (ants g))

-- | winner of a game
winnerGame :: Game -> AntNb
winnerGame g = antNb (head $ sortBy (\ a a' -> compare (score a) (score a')) (ants g)) -- TODO verify that 1 is winning in case of tie

-- | run a set of games
runMatch :: StdGen -> [Grid -> Direction] -> (AntNb, [Game])
runMatch gen moves = undefined
--  game = runGame $ initGame gen moves

-- | ant winner of a match
winnerMatch :: [Game] -> AntNb
winnerMatch = undefined

-- | tournament between mutiple ants
tournament = undefined

-- | save a game to the filesystem
saveGame :: Grid -> IO ()
saveGame = undefined
