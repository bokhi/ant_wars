-- | This module contains functions to run matches between different implementations of ant behaviours
module Game (Game(..)
            , initGame
            ,  runGame
            , saveGame
            , tournament
            ) where

import Grid
import Ant

data Game = Game {initialGrid :: Grid
                 , grid :: Grid
                 , ants :: [Ant]
                 }

-- | Initialise a game
initGame = undefined                   

-- | Given a game and a specific ant, update the grid by moving the ant
updateGame :: Game -> AntNb -> Game
updateGame g a = 
  Game (initialGrid g) gr' (replaceNth (pred a) ant' (ants g))
    where
      updateAnt a g = 
        let direction = move a $ g
            g' = updateGrid g (antNb a) direction  in
        (Ant (antNb a) (direction:(directions a)) (score a + (foodLeft g - foodLeft g')) (antCollision g') (move a), g')
      ant = ants g !! (pred a)
      gr = grid g
      (ant', gr') = updateAnt ant gr

                   
-- | run a game between two ants     
runGame = undefined 

-- | tournament between mutiple ants
tournament = undefined

-- | save a game to the filesystem
saveGame :: Grid -> IO ()
saveGame = undefined
