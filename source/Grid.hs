-- | This module contains the representation of a type Grid as well as functions to generate and modify grids
module Grid (Grid(..)
            , generateGrid
            , updateGrid
            , rotateGrid
            , foodLeft
            , fovGrid
            , Move
            , Ant
            ) where 

import System.Random
import Data.List

dimension = 11
nbFood = 15
fov = 5

-- | The food is represented as a list of points on the grid, and the ants as two separate points
data Grid = Grid { food :: [(Int, Int)]
                 , ant1 :: (Int, Int)
                 , ant2 :: (Int, Int)
                 } 
                            
-- | This function generates a dimension*dimension grid containing nbFood pieces of food             
generateGrid :: StdGen -> Grid                            
generateGrid gen = Grid food (5,  2) (5, 8) 
  where random = randomRs (0, (pred dimension)) gen :: [Int]
        food = generate random []
        generate random l = -- exactly nbFood pieces are wanted, they have to differ from each others and from the ants' positions
          if length l == nbFood
          then l
          else let (x:x':xs) = random in 
          if (x, x') `elem` l || (x, x') == (5, 2) || (x, x') == (5, 8)
          then generate xs l
          else generate xs ((x, x'):l)


replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs
                   
coord (x, y) = (succ dimension)*2*(2*x + 1) + 2*y + 1 -- translate a point to its position in the ascii table

showGrid :: Grid -> String
showGrid g = 
  let row  = intersperse '-' (replicate (succ dimension) '+') ++ "\n"
      column  = intersperse ' ' (replicate (succ dimension) '|') ++ "\n"
      grid = intercalate column (replicate (succ dimension) row)
      grid' = foldl (\ g pos -> replaceNth (coord pos) 'F' g) grid (food g)
      grid'' = replaceNth (coord $ ant1 g) '1' grid'
      grid''' = replaceNth (coord $ ant2 g) '2' grid''
  in grid'''
     
-- | The grid can be represented as a ASCII table, F for pieces of food, 1 and 2 for ant1 and ant2 respectively     
instance Show Grid where
  show g = showGrid g

-- | to express the motion an ant is able to do
data Move = NW | N | NE | E | SE | S | SW | W deriving Eq

-- | to differentiate the two ants
data Ant = Ant1 | Ant2 deriving Eq

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

-- | This function generates a new grid following the move of a specific ant
updateGrid :: Grid -> Ant ->  Move -> Grid
updateGrid g a m  = 
  if a == Ant1
  then Grid food' p (ant2 g)
  else Grid food' (ant1 g) p
    where
      f = food g
      p = updatePos (if a == Ant1 then ant1 g else ant2 g) m
      food' = delete p f

-- | 90' rotation of a grid
rotateGrid :: Grid -> Grid
rotateGrid g = Grid (map rotate (food g)) (rotate $ ant1 g) (rotate $ ant2 g)
  where rotate (x, y) = (y, mod (-x - 1) dimension)      
        
-- | Number of pieces of food on a grid        
foodLeft g = length $ food g

-- | Return the grid an ant perceives according to the fov
fovGrid :: Grid -> Ant -> Grid
fovGrid g a = Grid (filter f $ food g) (ant1 g) (ant2 g)
  where f (x, y) = 
          let (x', y') = if a == Ant1 then ant1 g else ant2 g 
              x'' = abs $ x - x'
              y'' = abs $ y - y' 
              fov' = fov `div` 2
          in
          (min x'' $ dimension - x'') <= fov' && (min y'' $ dimension - y'') <= fov'
        
          
           