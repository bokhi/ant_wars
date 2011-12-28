-- | This module implements different strategies to play the Ant wars game
module Ant (Ant(..)
           , initAnt

           ) where

import Grid

data Ant = Ant {antNb :: AntNb
               , directions :: [Direction]
               , score :: Int
               , kill :: Bool
               , move :: Grid -> Direction}

-- | Initiate an ant given a move function and an id
initAnt :: AntNb -> (Grid -> Direction) -> Ant
initAnt a m = Ant a [] 0 False m

              
-- | find the nearest piece of food
gready :: Grid -> Ant -> Direction
gready = undefined -- don't forget to call the fovGrid function

            