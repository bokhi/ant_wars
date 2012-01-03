-- | This module contains the representation of a type Grid as well as functions to generate and modify grids
module Grid (Grid(..)
            , Direction(..)
            , AntNb
            , generateGrids
            , updateGrid
            , rotateGrid
            , foodLeft
            , fovGrid
            , antCollision
            ) where 

import System.Random
import Data.List
import Data.Char
import Helper

dimension = 11
nbFood = 15
fov = 5
antInitialPosition = [(5,  2), (5, 8)]

-- | The food and ants are represented as lists of points on the grid
data Grid = Grid { food :: [(Int, Int)]
                 , antPositions :: [(Int, Int)]
                 } 
                            
-- | to express the motion an ant is able to do
data Direction = NW | N | NE | E | SE | S | SW | W deriving (Show, Eq)

type AntNb = Int

-- | Generate an infinite number of dimension*dimension grids containing nbFood pieces of food
generateGrids :: StdGen -> [Grid]
generateGrids gen = generateGrids' (randomRs (0, (pred dimension)) gen :: [Int])
  where
      generateFood random l = -- exactly nbFood pieces are wanted, they have to differ from each others and from the ants' positions
        if length l == nbFood
        then (l, random)
        else let (x:x':xs) = random in 
        if (x, x') `elem` l || (x, x') == (5, 2) || (x, x') == (5, 8)
        then generateFood xs l
        else generateFood xs ((x, x'):l)
      generateGrids' random = Grid food antInitialPosition : generateGrids' random'
        where (food, random') = generateFood random []

-- | The grid can be represented as a ASCII table, F for pieces of food, 1 and 2 for ant1 and ant2 respectively     
instance Show Grid where
  show g =   
    let coord (x, y) = (succ dimension)*2*(2*x + 1) + 2*y + 1 -- translate a point to its position in the ascii table
        row  = intersperse '-' (replicate (succ dimension) '+') ++ "\n"
        column  = intersperse ' ' (replicate (succ dimension) '|') ++ "\n"
        grid = intercalate column (replicate (succ dimension) row)
        grid' = foldl (\ g pos -> replaceNth (coord pos) 'F' g) grid (food g) -- add the food
        (grid'', _) = foldl (\ (g, n) pos -> (replaceNth (coord pos) (intToDigit n) g, succ n)) (grid', 1) (antPositions g) -- add the ants
    in grid''

-- | This function generates a new grid following the move of a specific ant
updateGrid :: Grid -> AntNb -> Direction -> Grid
updateGrid g n m  = 
  Grid food' (replaceNth (pred n) pos' (antPositions g))
    where
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
      updateFood f p m = delete (updatePos p m) f
      pos' = updatePos (antPositions g !! (pred n)) m
      food' = delete pos' $ food g

-- | 90' clockwise rotation of the grid
rotateGrid :: Grid -> Grid
rotateGrid g = Grid (map rotate (food g)) (map rotate (antPositions g))
  where rotate (x, y) = (y, mod (-x - 1) dimension)      
        
-- | Number of pieces of food on a grid        
foodLeft g = length $ food g

-- | Collision between the ants
antCollision g = antPositions g !! 0 == antPositions g !! 1 -- only works for two ants

-- | Return the grid an ant perceives according to the fov
fovGrid :: Grid -> AntNb -> Grid
fovGrid g n = Grid (filter f $ food g) (filter f $ antPositions g)
  where f (x, y) = 
          let (x', y') = antPositions g !! (pred n)
              x'' = abs $ x - x'
              y'' = abs $ y - y' 
              fov' = fov `div` 2
          in (min x'' $ dimension - x'') <= fov' && (min y'' $ dimension - y'') <= fov'
           
        
          
           