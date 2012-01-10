-- | This module implements different strategies to play the Ant wars game
module Ant (Ant(..)
           , initAnt
           , updateAnt
           , testMove
           , gready
           , gready'
           , user
           , predator
           , predator'
           , hider
           , hider'
           , wise
           , wise'
           , precautionary
           , precautionary'
           , geneticAnt
           , ruleBasedAnts
           , ruleBasedAnts'
           ) where

import Data.List
import System.IO.Unsafe
import Grid
import Memory
import Genetic

ruleBasedAnts = [testMove, gready, predator, hider, wise, precautionary] -- list of rule-based ants developed
ruleBasedAnts' = [gready', predator', hider', wise', precautionary'] -- the ' denote the fact that the ants use their memory to act

data Ant = Ant {antNb :: AntNb -- a number qualifying the ant : 0 or 1
               , directions :: [Direction] -- direction moved to from the initial position - reverse order
               , kill :: Bool -- whether or not the ant has killed its opponent
               , move :: Memory -> Grid -> Direction -- the function used to find the ant's next move on the grid
               , memory :: Memory -- a data structure containing information the ant has learned from previous moves
               }
           
instance Show Ant where
  show a = "Ant : " ++ show (antNb a) ++ "\n" ++ "Directions : " ++ show (reverse (directions a)) ++ "\n"


-- | Initiate an ant given a move function and an id
initAnt :: AntNb -> (Int, Int) ->  (Memory -> Grid -> Direction) -> Ant
initAnt a p m = Ant a [] False m (initMemory p a)

-- | Move an ant on a grid to update the ant and grid structure
updateAnt :: Ant -> Grid -> (Ant, Grid)
updateAnt a g = (Ant (antNb a) (direction:(directions a)) (collision || kill a) (move a) memory' , g') -- careful not to update a dead ant
  where 
    memory' = updateMemory (memory a) (antNb a) g
    direction = (move a) (memory a) (fovGrid g (antNb a)) -- the fov is applied to the grid
    g' = updateGrid g (antNb a) direction  
    collision = length (antPositions g) /= length (antPositions g')

-- | move test function
testMove :: AntNb -> Memory -> Grid -> Direction
testMove a m g = NW
              
-- | find the nearest piece of food
gready :: AntNb -> Memory -> Grid -> Direction
gready a m g = if null (food g) then NW else d
  where
    aPos = antPosition g a
    distanceFood d = minimum $ map (distance (updatePos aPos d)) (food g) -- distance to the nearest piece of food given a direction
    ds = [N, W, S, E, NE, NW, SW, SE]
    d = minimumBy (\ d d' -> compare (distanceFood d) (distanceFood d')) ds 
    
gready' :: AntNb -> Memory -> Grid -> Direction    
gready' a m g = gready a m (memoryGrid (updateMemory m a g) g)
    
-- | user input function        
user :: AntNb -> Memory -> Grid -> Direction
user a m g = unsafePerformIO $ do
  print g'
  d <- getLine
  return (read d :: Direction)
  where
    g' = fovGrid g a -- the user have to use its own memory :), the grid is thus reset to the fov
    
-- | pursue the opponent if present within the fov, otherwise move towards the nearest piece of food
predator :: AntNb -> Memory -> Grid -> Direction
predator a m g = 
  if length (antPositions g) > 1 -- if there is a prey
  then d' 
  else gready a m g
    where
      aPos = antPosition g a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g (succ a)) -- distance to the prey given a direction
      d' = minimumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds
    
predator' a m g = predator a m (memoryGrid (updateMemory m a g) g)
      
-- | try to escape if an opponent is present within the fov, otherwise gready strategy
hider :: AntNb -> Memory -> Grid -> Direction
hider a m g = 
  if length (antPositions g) > 1 -- if there is a predator
  then d'
  else gready a m g
    where   
      aPos = antPosition g a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePredator d = distance (updatePos aPos d) (antPosition g (succ a)) -- distance to the predator given a direction
      d' = maximumBy (\ d d' -> compare (distancePredator d) (distancePredator d')) ds
      
hider' a m g = hider a m (memoryGrid (updateMemory m a g) g)
      
-- | choose to escape if the opponent is closer than the closest piece of food      
wise :: AntNb -> Memory -> Grid -> Direction      
wise a m g = 
  if length (antPositions g) > 1 && not (null (food g))
  then 
    if distanceOpponent < distanceFood
    then hider a m g
    else gready a m g
  else gready a m g
    where   
      aPos = antPosition g a
      distanceFood = minimum $ map (distance aPos) (food g)
      distanceOpponent = distance aPos (antPosition g (succ a))

wise' a m g = wise a m (memoryGrid (updateMemory m a g) g)

-- | avoid to being killed before a certain score 
precautionary :: AntNb -> Memory -> Grid -> Direction      
precautionary a m g = 
  if length (antPositions g') > 1 && score g !! a < nbFood `div` 2 
  then wise a m g
  else gready a m g
    where g' = fovGrid g a
          
precautionary' a m g = precautionary a m (memoryGrid (updateMemory m a g) g)

-- | The I expression trees are used to evaluate the result of N and NE movement on the grid 
-- rotation properties of the problem are thus exploited
geneticAnt :: (I, I) -> AntNb -> Memory -> Grid -> Direction 
geneticAnt (t, t') _ _ g = d
  where
    g' = rotateGrid g
    g'' = rotateGrid g'
    g''' = rotateGrid g''
    gs = [g, g', g'', g''']
    ds = [N, W, S, E, NE, NW, SW, SE]
    es = map (evalI t) gs
    es' = map (evalI t') gs
    (_, d) = maximumBy (\ x x' -> compare (fst x) (fst x')) (zip (es ++ es') ds) -- the greatest number gives us the direction to go to 
