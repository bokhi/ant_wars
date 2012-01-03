-- | This module implements different strategies to play the Ant wars game
module Ant (Ant(..)
           , initAnt
           , updateAnt
           , testMove
           ) where

import Grid

data Ant = Ant {antNb :: AntNb
               , directions :: [Direction]
               , score :: Int
               , kill :: Bool
               , move :: Grid -> Direction}
           
instance Show Ant where
  show a = "Ant : " ++ show (antNb a) ++ "\n" ++ "Score : " ++ show (score a) ++ "\n" ++ "Directions : " ++ show (reverse (directions a)) ++ "\n"


-- | Initiate an ant given a move function and an id
initAnt :: AntNb -> (Grid -> Direction) -> Ant
initAnt a m = Ant a [] 0 False m

-- | Move an ant on a grid to update the ant and grid structure
updateAnt :: Ant -> Grid -> (Ant, Grid)
updateAnt a g = 
  let direction = move a $ g
      g' = updateGrid g (antNb a) direction  
  in (Ant (antNb a) (direction:(directions a)) (score a + (foodLeft g - foodLeft g')) (antCollision g') (move a), g')


-- initAnt

-- | move test function
testMove :: Grid -> Direction
testMove g = NW
              
-- | find the nearest piece of food
gready :: Grid -> Ant -> Direction
gready = undefined -- don't forget to call the fovGrid function

            