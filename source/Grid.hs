-- | This module contains the representation of a type Grid as well as functions to generate and modify grids
module Grid (Grid(..)
            , Direction(..)
            , AntNb
            , dimension
            , fov
            , antInitialPositions
            , generateGrids
            , antPosition
            , updateGrid
            , rotateGrid
            , foodLeft
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

-- | The food and ants are represented as lists of points on the grid
-- eaten food and killed ants are removed from the lists
data Grid = Grid { food :: [(Int, Int)]
                 , antPositions :: [(Int, Int)]
                 } 
                            
-- | to express the motion an ant is able to do
data Direction = NW | N | NE | E | SE | S | SW | W deriving (Show, Eq, Read)

-- | a number associated to ant, 0 for ant1, 1 for ant2
type AntNb = Int

-- | Generate an infinite number of dimension*dimension grids containing nbFood pieces of food
generateGrids :: StdGen -> [Grid]
generateGrids gen = generateGrids' (randomRs (0, (pred dimension)) gen :: [Int])
  where
    generateFood random l = -- exactly nbFood pieces are wanted, they have to differ from each other and from the ants' positions
        if length l == nbFood
        then (l, random)
        else 
          let (x:x':xs) = random in 
          if (x, x') `elem` l || (x, x') `elem` antInitialPositions 
          then generateFood xs l
          else generateFood xs ((x, x'):l)
    generateGrids' random = (Grid food antInitialPositions):generateGrids' random'
        where (food, random') = generateFood random []

-- | The grid can be represented as a ASCII table, F for pieces of food, 0 and 1 for ant0 and ant1 respectively     
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

-- | This function generates a new grid following the move of a specific ant
updateGrid :: Grid -> AntNb -> Direction -> Grid
updateGrid g a m  = 
  if length (antPositions g) > 1 -- two player
  then 
    if collision 
    then Grid (food g) [pos'] -- only one ant left
    else Grid food' (replaceNth a pos' (antPositions g))
  else Grid food' [pos']
    where
      updateFood f p m = delete (updatePos p m) f
      pos' = updatePos (antPosition g a) m
      food' = delete pos' $ food g
      collision = length (antPositions g) > 1 && pos' == antPosition g (succ a)
      
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

-- | 90' clockwise rotation of the grid
rotateGrid :: Grid -> Grid
rotateGrid g = Grid (map rotate (food g)) (map rotate (antPositions g))
  where rotate (x, y) = (y, mod (-x - 1) dimension)      
        
-- | Number of pieces of food on a grid        
foodLeft g = length $ food g

-- | Return the grid an ant perceives according to the fov
fovGrid :: Grid -> AntNb -> Grid
fovGrid g a = Grid (filter f $ food g) (filter f $ antPositions g)
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