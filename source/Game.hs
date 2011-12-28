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
                   
-- | run a game between two ants     
runGame = undefined 

-- | tournament between mutiple ants
tournament = undefined

-- | save a game to the filesystem
saveGame = undefined
