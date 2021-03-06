-- | This module contains the representation of a type Grid as well as functions to generate and modify grids
module Grid (Grid(..)
            , Direction(..)
            , AntNb
            , dimension
            , nbFood
            , fov
            , antInitialPositions
            , generateGrids
            , antPosition
            , updateGrid
            , rotateGrid
            , fovGrid
            , distance
            , updatePos
            ) where 

import System.Random
import Data.List
import Data.Char
import Helper

dimension = 11 -- dimension of a grid
nbFood = 15 -- initial number of pieces of food on a grid
fov = 5 -- size of the field of view square 
antInitialPositions = [(5,  2), (5, 8)] -- initial position of the ants on the grid
antInitialScore = [0, 0] -- ants' initial score

-- | The food and ants are represented as lists of points on the grid
-- eaten food and killed ants are removed from the lists
data Grid = Grid { food :: [(Int, Int)] 
                 , antPositions :: [(Int, Int)] -- when the fov is applied, it may contain only one element : the playing ant position - if more ants then antn is n-nth element of the list
                 , score :: [Int] -- ant-n score is the n-nth element of the list
                 } 
                            
-- | to express the motion an ant is able to do
data Direction = NW | N | NE | E | SE | S | SW | W deriving (Show, Eq, Read)

-- | a number associated to ant, 0 for ant1, 1 for ant2
type AntNb = Int

-- | Generate an infinite number of dimension*dimension grids containing nbFood pieces of food
generateGrids :: StdGen -> [Grid]
generateGrids gen = generateGrid' gen
  where 
    generateFood gen random l n = 
      if length l == nbFood
      then l
      else generateFood g' random' (x:l) (n - 1)
        where
          (g, g') = split gen 
          (x, random') = removeNth (fst (randomR (0, n-1) g) :: Int) random
    cs = [(x, y) | x <- [0..(pred dimension)], y <- [0..(pred dimension)], (x, y) /= antInitialPositions !! 0, (x, y) /= antInitialPositions !! 1]
    generateGrid' gen = Grid (generateFood g cs [] (length cs)) antInitialPositions antInitialScore : generateGrid' g'
      where
        (g, g') = split gen

-- | The grid can be represented as a ASCII table, F for pieces of food, 0 and 1 for ant0 and ant1 respectively     
-- the ant playing is always represented by a 0 and the opponent by a 1              
instance Show Grid where
  show g =   
    let coord (x, y) = (succ dimension)*2*(2*x + 1) + 2*y + 1 -- translate a point to its position in the ascii table
        row  = intersperse '-' (replicate (succ dimension) '+') ++ "\n"
        column  = intersperse ' ' (replicate (succ dimension) '|') ++ "\n"
        grid = intercalate column (replicate (succ dimension) row)
        grid' = foldl (\ g pos -> replaceNth (coord pos) 'F' g) grid (food g) -- add the food
        (grid'', _) = foldl (\ (g, n) pos -> (replaceNth (coord pos) (intToDigit n) g, succ n)) (grid', 0) (antPositions g) -- add the ants
    in grid''

-- | return the position of an ant
antPosition :: Grid -> AntNb -> (Int, Int)  
antPosition g a = antPositions g !! (a `mod` length (antPositions g)) 

-- | compute the new position on the grid given a direction
updatePos :: (Int, Int) -> Direction -> (Int, Int)
updatePos (x, y) m = 
        case m of 
          NW -> (mod (x - 1) dimension, mod (y - 1) dimension)
          N -> (mod (x - 1) dimension, y)
          NE -> (mod (x - 1) dimension, mod (y + 1) dimension)
          E -> (x, mod (y + 1) dimension)
          SE -> (mod (x + 1) dimension, mod (y + 1) dimension)
          S -> (mod (x + 1) dimension, y)
          SW -> (mod (x + 1) dimension, mod (y - 1) dimension)
          W -> (x, mod (y - 1) dimension)

-- | This function generates a new grid following the move of a specific ant
updateGrid :: Grid -> AntNb -> Direction -> Grid
updateGrid g a m  = 
  if length (antPositions g) > 1 -- two player
  then 
    if collision 
    then Grid (food g) [pos'] (score g) -- only one ant left
    else Grid food' (replaceNth a pos' (antPositions g)) score'
  else Grid food' [pos'] score'
    where
      pos' = updatePos (antPosition g a) m
      collision = length (antPositions g) > 1 && pos' == antPosition g (succ a)
      score' = replaceNth a (score g !! a + ((length (food g)) - (length food'))) (score g)
      food' = delete pos' $ food g
        
-- | Return the grid an ant perceives according to the fov
fovGrid :: Grid -> AntNb -> Grid
fovGrid g a = Grid (filter f $ food g) (filter f $ antPositions g) (score g)
  where 
    f pos = distance pos' pos <= fov'
    pos' =  antPosition g a
    fov' = fov `div` 2
        
-- | minimum number of moves between two positions on the grid             
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x, y) (x', y') =  max (min x'' $ dimension - x'') (min y'' $ dimension - y'')
  where
    x'' = abs $ x - x'
    y'' = abs $ y - y' 
    
-- | 90' clockwise rotation of the grid
rotateGrid :: Grid -> Grid
rotateGrid g = Grid (map rotate (food g)) (map rotate (antPositions g)) (score g)
  where rotate (x, y) = (y, mod (-x - 1) dimension)      
