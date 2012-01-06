-- | This module implements different strategies to play the Ant wars game
module Ant (Ant(..)
           , initAnt
           , updateAnt
           , testMove
           , gready
           , user
           , predator
           ) where

import Data.List
import System.IO.Unsafe
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
updateAnt a g = (Ant (antNb a) (direction:(directions a)) (score a + (foodLeft g - foodLeft g')) (collision || kill a) (move a), g') -- careful not to update a dead ant
  where 
    direction = move a $ g
    g' = updateGrid g (antNb a) direction  
    collision = length (antPositions g) /= length (antPositions g')



-- initAnt

-- | move test function
testMove :: Grid -> Direction
testMove g = NW
              
-- | find the nearest piece of food
gready :: AntNb -> Grid -> Direction
gready a g = if null (food g') then NW else d
  where
    g' =  fovGrid g a
    aPos = antPositions g !! (a `mod` length (antPositions g))
    distanceFood d = minimum $ map (distance (updatePos aPos d)) (food g')
    ds = [N, NW, NE, E, W, S, SE, SW]
    d = minimumBy (\ d d' -> compare (distanceFood d) (distanceFood d')) ds
            
-- | user input function        
user :: AntNb -> Grid -> Direction
user a g = unsafePerformIO $ do
  print g
  d <- getLine
  return (read d :: Direction)
  where
    g' = fovGrid g a
    
-- | pursue the opponent if present within the fov, otherwize move towards the nearest piece of food
predator :: AntNb -> Grid -> Direction
predator a g = 
  if length (antPositions g') >= 2 -- if there is a prey
  then d' 
  else if null (food g') then NW else d
  where
    g' = fovGrid g a
    aPos = antPositions g !! (a `mod` length (antPositions g))
    distanceFood d = minimum $ map (distance (updatePos aPos d)) (food g')
    ds = [N, NW, NE, E, W, S, SE, SW]
    d = minimumBy (\ d d' -> compare (distanceFood d) (distanceFood d')) ds
    distancePrey d = minimum $ map (distance (updatePos aPos d)) [antPositions g' !! (succ a `mod` length (antPositions g'))]
    d' = minimumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds