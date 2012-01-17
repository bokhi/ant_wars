-- | This module implements different strategies to play the Ant wars game
module Ant (Ant(..)
           , initAnt
           , updateAnt
           , testMove
           , greedy
           , greedy'
           , user
           , predator
           , predator'
           , hider
           , hider'
           , wise
           , wise'
           , precautionary
           , precautionary'
           , searcher
           , searcher'
           , greedySearcher
           , greedySearcher'
           , predatorSearcher
           , predatorSearcher'
           , hiderSearcher
           , hiderSearcher'
           , superSearcher
           , superSearcher'
           , geneticAnt
           , ruleBasedAnts
           , ruleBasedAnts'
           ) where

import Data.List
import Data.Ord
import System.IO.Unsafe
import Grid
import Memory
import Genetic

ruleBasedAnts = [testMove, greedy, predator, hider, wise, precautionary, searcher, greedySearcher, predatorSearcher, hiderSearcher, superSearcher]
ruleBasedAnts' = [greedy', predator', hider', wise', precautionary', searcher', greedySearcher', predatorSearcher', hiderSearcher', superSearcher']

data Ant = Ant {antNb :: AntNb
               , directions :: [Direction]
               , score :: Int
               , kill :: Bool
               , move :: Memory -> Grid -> Direction
               , memory :: Memory}
           
instance Show Ant where
  show a = "Ant : " ++ show (antNb a) ++ "\n" ++ "Score : " ++ show (Ant.score a) ++ "\n" ++ "Directions : " ++ show (reverse (directions a)) ++ "\n"


-- | Initiate an ant given a move function and an id
initAnt :: AntNb -> (Int, Int) ->  (Memory -> Grid -> Direction) -> Ant
initAnt a p m = Ant a [] 0 False m (initMemory p a)

-- | Move an ant on a grid to update the ant and grid structure
updateAnt :: Ant -> Grid -> (Ant, Grid)
updateAnt a g = (Ant (antNb a) (direction:(directions a)) (Ant.score a + (foodLeft g - foodLeft g')) (collision || kill a) (move a) memory' , g') -- careful not to update a dead ant
  where 
    memory' = updateMemory (memory a) (antNb a) g
    direction = (move a) (memory a) (fovGrid g (antNb a)) -- the fov is applied to the grid
    g' = updateGrid g (antNb a) direction  
    collision = length (antPositions g) /= length (antPositions g')

-- | move test function
testMove :: AntNb -> Memory -> Grid -> Direction
testMove a m g = NW
              
-- | find the nearest piece of food
greedy a m g = if null (food g') then NW else m
  where
    g' =  g
    aPos = antPosition g' a
    distanceFood d = minimum $ map (distance (updatePos aPos d)) (food g')
    ds = [N, W, S, E, NE, NW, SW, SE]
    m = minimumBy (\ d d' -> compare (distanceFood d) (distanceFood d')) ds
    
greedy' :: AntNb -> Memory -> Grid -> Direction    
greedy' a m g = greedy a m (memoryGrid (updateMemory m a g) g)
    
-- | user input function        
user :: AntNb -> Memory -> Grid -> Direction
user a m g = unsafePerformIO $ do
  print g'
  putStrLn $ "Choose between : " ++ show [N, W, S, E, NE, NW, SW, SE]
  d <- getLine
  return (read d :: Direction)
  where
    g' = fovGrid g a
    
-- | pursue the opponent if present within the fov, otherwise move towards the nearest piece of food
predator :: AntNb -> Memory -> Grid -> Direction
predator a m g = 
  if length (antPositions g') > 1 -- if there is a prey
  then d' 
  else greedy a m g
    where
      g' = g
      aPos = antPosition g' a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g' (succ a))
      d' = minimumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds
    
predator' a m g = predator a m (memoryGrid (updateMemory m a g) g)
      
-- | try to escape if an opponent is present within the fov, otherwise greedy strategy
hider :: AntNb -> Memory -> Grid -> Direction
hider a m g = 
  if length (antPositions g') > 1
  then d'
  else greedy a m g
    where   
      g' = fovGrid g a
      aPos = antPosition g' a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g' (succ a))
      d' = maximumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds
      
hider' a m g = hider a m (memoryGrid (updateMemory m a g) g)
      
-- | choose to escape if the opponent is closer than the closest piece of food      
wise :: AntNb -> Memory -> Grid -> Direction      
wise a m g = 
  if length (antPositions g') > 1 && not (null (food g'))
  then 
    if distanceOpponent < distanceFood
    then hider a m g
    else greedy a m g
  else greedy a m g
    where   
      g' = fovGrid g a
      aPos = antPosition g' a
      distanceFood = minimum $ map (distance aPos) (food g')
      distanceOpponent = distance aPos (antPosition g' (succ a))

wise' a m g = wise a m (memoryGrid (updateMemory m a g) g)

-- | avoid to being killed before a certain score 
precautionary :: AntNb -> Memory -> Grid -> Direction      
precautionary a m g = 
  if length (antPositions g') > 1 && Grid.score g !! a < nbFood `div` 2
  then wise a m g
  else greedy a m g
    where g' = fovGrid g a
          
precautionary' a m g = precautionary a m (memoryGrid (updateMemory m a g) g)

geneticAnt :: (I, I) -> AntNb -> Memory -> Grid -> Direction 
geneticAnt (t, t') _ _ g = genAnt (t, t') g

-- | look ahead five moves and choose direction of path with the most food
searcher :: AntNb -> Memory -> Grid -> Direction
searcher a m g = if null (food g) then NW else d
  where
    aPos = antPosition g a
    ds = [N, W, S, E, NE, NW, SW, SE]
    nPos d n = ((updatePos ((fst aPos + n), (snd aPos)) d), d)
    foody d = zip (food g) (replicate (length (food g)) d) -- Keep track of d
    pInt d = (map (nPos d) [1..5])++(intersect (foody d) (map (nPos d) [1..5]))
    d' = maximumBy (comparing length) (map pInt ds)
    d = if length d' > length (map (nPos N) [1..5]) then snd (d' !! 0) else greedy a m g

searcher' a m g = searcher a m (memoryGrid (updateMemory m a g) g)

-- | greedy with look ahead
greedySearcher :: AntNb -> Memory -> Grid -> Direction
greedySearcher a m g = if null (food g) then NW else d
  where
    aPos = antPosition g a
    ds = [N, W, S, E, NE, NW, SW, SE]
    distanceFood d = minimum $ map (distance (updatePos aPos d)) (food g)
    mdi = minimum $ map (distanceFood) ds
    md = minimumBy (\ d d' -> compare (distanceFood d) (distanceFood d')) ds
    d' = searcher a m g
    d = if (mdi < 2) then md else d' -- if food is near then move towards it

greedySearcher' a m g = greedySearcher a m (memoryGrid (updateMemory m a g) g)

-- | predator with look ahead
predatorSearcher :: AntNb -> Memory -> Grid -> Direction
predatorSearcher a m g = 
  if length (antPositions g) > 1
  then d'
  else greedySearcher a m g
    where
      aPos = antPosition g a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g (succ a)) -- distance to the prey given a direction
      d' = minimumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds

predatorSearcher' a m g = predatorSearcher a m (memoryGrid (updateMemory m a g) g)

-- | hider with look ahead
hiderSearcher :: AntNb -> Memory -> Grid -> Direction
hiderSearcher a m g =
  if length (antPositions g') > 1
  then d'
  else greedySearcher a m g
    where
      g' = fovGrid g a
      aPos = antPosition g' a
      ds = [N, W, S, E, NE, NW, SW, SE]
      distancePrey d = distance (updatePos aPos d) (antPosition g' (succ a))
      d' = maximumBy (\ d d' -> compare (distancePrey d) (distancePrey d')) ds

hiderSearcher' a m g = hiderSearcher a m (memoryGrid (updateMemory m a g) g)

-- | test function
superSearcher :: AntNb -> Memory -> Grid -> Direction
superSearcher a m g = d
  where
    aPos = antPosition g a
    --ePos d = ((antPosition g (succ a)), d)
    ds = [N, W, S, E, NE, NW, SW, SE]
    -- nPos d n = ((updatePos ((fst aPos + n), (snd aPos)) d), d)
    -- pInt d = (map (nPos d) [1..3])++(intersect [(ePos d)] (map (nPos d) [1..3]))
    -- mp = maximumBy (comparing length) (map pInt ds)
    -- INTERSECTION WITH FOOD
    nPos d n = ((updatePos ((fst aPos + n), (snd aPos)) d), d)
    foody d = zip (food g) (replicate (length (food g)) d) -- Keep track of d
    pInt d = (map (nPos d) [1..5])++(intersect (foody d) (map (nPos d) [1..5]))
    mp = maximumBy (comparing length) (map pInt ds)
    as = Grid.score g !! a
    distancePrey d = distance (updatePos aPos d) (antPosition g (succ a))
    md = minimum  (map distancePrey ds)
    td = if ((length mp) - 5) > 1 then greedySearcher a m g else predatorSearcher a m g
    d' = if (md < 1) then predatorSearcher a m g else greedySearcher a m g
    d = if (as < 8) then d' else td --greedy a m g -- end game strategy

superSearcher' a m g = superSearcher a m (memoryGrid (updateMemory m a g) g)