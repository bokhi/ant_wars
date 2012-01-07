-- | This module implements different strategies to play the Ant wars game
module Ant (Ant(..)
           , initAnt
           , updateAnt
           , testMove
           , gready
           , user
           , predator
           , hider
           , wise
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
  show a = "Ant : " ++ show (antNb a) ++ "\n" ++ "Score : " ++ show (Ant.score a) ++ "\n" ++ "Directions : " ++ show (reverse (directions a)) ++ "\n"


-- | Initiate an ant given a move function and an id
initAnt :: AntNb -> (Grid -> Direction) -> Ant
initAnt a m = Ant a [] 0 False m

-- | Move an ant on a grid to update the ant and grid structure
updateAnt :: Ant -> Grid -> (Ant, Grid)
updateAnt a g = (Ant (antNb a) (direction:(directions a)) (Ant.score a + (foodLeft g - foodLeft g')) (collision || kill a) (move a), g') -- careful not to update a dead ant
  where 
    direction = move a $ g
    g' = updateGrid g (antNb a) direction  
    collision = length (antPositions g) /= length (antPositions g')

-- | move test function
testMove :: AntNb -> Grid -> Direction
testMove a g = NW
              
-- | find the nearest piece of food
gready :: AntNb -> Grid -> Direction
gready a g = if null (food g') then NW else m
  where
    g' =  fovGrid g a
    aPos = antPosition g' a
    distanceFood d = minimum $ map (distance (updatePos aPos d)) (food g')
    ds = [N, W, S, E, NE, NW, SW, SE]
    m = minimumBy (\ d d' -> compare (distanceFood d) (distanceFood d')) ds
            
-- | user input function        
user :: AntNb -> Grid -> Direction
user a g = unsafePerformIO $ do
  print g'
  d <- getLine
  return (read d :: Direction)
  where
    g' = fovGrid g a
    
-- | pursue the opponent if present within the fov, otherwise move towards the nearest piece of food
predator :: AntNb -> Grid -> Direction
predator a g = 
  if length (antPositions g') > 1 -- if there is a prey
  then d' 
  else gready a g
    where
      g' = fovGrid g a
      aPos = antPosition g' a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g' (succ a))
      d' = minimumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds
    
-- | try to escape if an opponent is present within the fov, otherwise gready strategy
hider :: AntNb -> Grid -> Direction
hider a g = 
  if length (antPositions g') > 1
  then d'
  else gready a g
    where   
      g' = fovGrid g a
      aPos = antPosition g' a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g' (succ a))
      d' = maximumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds
      
-- | choose to escape if the opponent is closer than the closest piece of food      
wise :: AntNb -> Grid -> Direction      
wise a g = 
  if length (antPositions g') > 1 && not (null (food g'))
  then 
    if distanceOpponent < distanceFood
    then hider a g
    else gready a g
  else gready a g
    where   
      g' = fovGrid g a
      aPos = antPosition g' a
      distanceFood = minimum $ map (distance aPos) (food g')
      distanceOpponent = distance aPos (antPosition g' (succ a))

  
      