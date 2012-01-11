-- | This module is aimed at generate genetically programmed ants
module Genetic (I(..)
                , evalI
                , generateI
                ) where


import System.Random
import Data.List
import Grid
import Memory

-- | (x, y, dx, dy) is a rectangle starting at position (x, y) on the grid and of length dx and dy
type Rect = (Int, Int, Int, Int)

dx = 4 -- maximum dx size of a rectangle
dy = 4 -- maximum dy size of a rectangle

-- | Represent boolean function
data B = And B B
       | Or B B
       | Not B
       | IsFood Rect
       | IsEnemy Rect
       | IsSmaller I I deriving Show

-- | Represent integer function
data I = Const Int
       | Add I I
       | Sub I I
       | Mul I I
       | If B I I deriving Show
                           
-- | number of nodes in a B expression
nodeB :: B -> Int
nodeB b = case b of
  And b1 b2 -> succ $ nodeB b1 + nodeB b2
  Or b1 b2 -> succ $ nodeB b1 + nodeB b2
  Not b' -> succ $ nodeB b'
  IsFood _ -> 1
  IsEnemy _ -> 1
  IsSmaller i1 i2 -> succ $ nodeI i1 + nodeI i2
  
-- | number of nodes in a B expression  
nodeI :: I -> Int
nodeI i = case i of
  Const _ -> 1
  Add i1 i2 -> succ $ nodeI i1 + nodeI i2
  Sub i1 i2 -> succ $ nodeI i1 + nodeI i2
  Mul i1 i2 -> succ $ nodeI i1 + nodeI i2
  If b i1 i2 -> succ $ nodeB b + nodeI i1 + nodeI i2
  
-- | depth of a B expression
depthB :: B -> Int
depthB b = case b of
  And b1 b2 -> succ $ depthB b1 `max` depthB b2
  Or b1 b2 -> succ $ depthB b1 `max` depthB b2
  Not b' -> succ $ depthB b'
  IsFood _ -> 1
  IsEnemy _ -> 1
  IsSmaller i1 i2 -> succ $ depthI i1 `max` depthI i2
  
-- | depth of a I expression
depthI :: I -> Int  
depthI i = case i of
  Const _ -> 1
  Add i1 i2 -> succ $ depthI i1 `max` depthI i2
  Sub i1 i2 -> succ $ depthI i1 `max` depthI i2
  Mul i1 i2 -> succ $ depthI i1 `max` depthI i2
  If b i1 i2 -> succ $ depthB b `max` depthI i1 `max` depthI i2
                           
-- | replace b2 in b1 at position pos                
replaceBB :: Int -> B -> B -> Int -> B  
replaceBB pos b1 b2 n = replace' pos b1 b2 n
  where 
    replace' pos b1 b2 n
      | pos == n = b2
      | otherwise = case b1 of
        And b1' b2' -> And (replace' pos b1' b2 (succ n)) (replace' pos b2' b2 (succ n + nodeB b1'))
        Or b1' b2' -> Or (replace' pos b1' b2 (succ n)) (replace' pos b2' b2 (succ n + nodeB b1'))
        Not b -> Not (replace' pos b b2 (succ n))
        IsFood x -> IsFood x
        IsEnemy x -> IsEnemy x
        IsSmaller i1 i2 -> IsSmaller (replaceIB pos i1 b2 (succ n)) (replaceIB pos i2 b2 (succ n + nodeI i1))
                
-- | replace i in b at position pos                
replaceBI :: Int -> B -> I -> Int -> B  
replaceBI pos b i n = replace' pos b i n
  where
    replace' pos b i n = case b of
      And b1 b2 -> And (replace' pos b1 i (succ n)) (replace' pos b2 i (succ n + nodeB b1))
      Or b1 b2 -> Or (replace' pos b1 i (succ n)) (replace' pos b2 i (succ n + nodeB b1))
      Not b' -> Not (replace' pos b' i (succ n))
      IsFood x -> IsFood x
      IsEnemy x -> IsEnemy x
      IsSmaller i1 i2 -> IsSmaller (replaceII pos i1 i (succ n)) (replaceII pos i2 i (succ n + nodeI i1))
  

-- | replace i2 in i1 at position pos                            
replaceII :: Int -> I -> I -> Int -> I                           
replaceII pos i1 i2 n = replace' pos i1 i2 n
  where
    replace' pos i1 i2 n 
      | pos == n = i2
      | otherwise = case i1 of
        Const i -> Const i
        Add i1' i2' -> Add (replace' pos i1' i2 (succ n)) (replace' pos i2' i2 (succ n + nodeI i1'))
        Sub i1' i2' -> Sub (replace' pos i1' i2 (succ n)) (replace' pos i2' i2 (succ n + nodeI i1'))
        Mul i1' i2' -> Mul (replace' pos i1' i2 (succ n)) (replace' pos i2' i2 (succ n + nodeI i1'))
        If b i1' i2' -> If (replaceBI pos b i2 (succ n)) (replace' pos i1' i2 (succ n + nodeB b)) (replace' pos i2' i2 (succ n + nodeB b + nodeI i1))
        
-- | replace b in i at position pos        
replaceIB :: Int -> I -> B -> Int -> I
replaceIB pos i b n = replace' pos i b n
  where
    replace' pos i b n = case i of
      Const i -> Const i
      Add i1 i2 -> Add (replace' pos i1 b (succ n)) (replace' pos i2 b (succ n + nodeI i1))
      Sub i1 i2 -> Sub (replace' pos i1 b (succ n)) (replace' pos i2 b (succ n + nodeI i1))
      Mul i1 i2 -> Mul (replace' pos i1 b (succ n)) (replace' pos i2 b (succ n + nodeI i1))
      If b' i1 i2 -> If (replaceBB pos b' b (succ n)) (replace' pos i1 b (succ n + nodeB b')) (replace' pos i2 b (succ n + nodeB b' + nodeI i1))
                           
-- | evaluate a B expression                           
evalB :: B -> AntNb -> Memory -> Grid -> Bool
evalB (And b1 b2) a m g = evalB b1 a m g && evalB b2 a m g
evalB (Or b1 b2) a m g = evalB b1 a m g || evalB b2 a m g
evalB (Not b) a m g = not $ evalB b a m g
evalB (IsFood r) _ m g = not $ null $ food g `intersect` rect2List r
evalB (IsEnemy r) a m g = if length (antPositions g) == 1 
                      then False
                      else not $ null $ [antPositions g !! (succ a `mod` 2)] `intersect` rect2List r -- test whether the enemy is within the rectangle
evalB (IsSmaller f1 f2) a m g = evalI f1 a m g < evalI f2 a m g

-- | evaluate a F expression
evalI :: I -> AntNb -> Memory -> Grid -> Int
evalI (Const f) a m g = f
evalI (Add f1 f2) a m g = evalI f1 a m g + evalI f2 a m g
evalI (Sub f1 f2) a m g = evalI f1 a m g - evalI f2 a m g
evalI (Mul f1 f2) a m g = evalI f1 a m g * evalI f2 a m g
evalI (If b f1 f2) a m g = if evalB b a m g then evalI f1 a m g else evalI f2 a m g

-- | Convert a type A to the List of positions that constitute the rectangle 
rect2List :: Rect -> [(Int, Int)]
rect2List (x, y, dx, dy) = [(x' `mod` dimension , y' `mod` dimension) | x' <- [x..(x+dx)], y' <- [y..(y+dy)]]


-- | 
generateB :: StdGen -> Int -> B            
generateB gen d 
  | d <= 2 = if fst (random gen ) :: Bool then IsFood rect else IsEnemy rect
  | otherwise = case fst (randomR (0, 5) gen) :: Int of 
    0 -> And (generateB g $ pred d) (generateB g' $ pred d)
    1 -> Or (generateB g $ pred d) (generateB g' $ pred d)
    2 -> Not (generateB g $ pred d)
    3 -> IsFood rect
    4 -> IsEnemy rect
    5 -> IsSmaller (generateI g $ pred d) (generateI g' $ pred d) 
    where 
      (a:b:c:d:_) = randoms g
      rect = (a `mod` dimension, b `mod` dimension, c `mod` dx, d `mod` dy)
      (g, g') = split gen

-- | given a seed, construct a I expression of height h
generateI :: StdGen -> Int -> I
generateI gen d 
  | d <= 2 = Const x 
  | otherwise = case fst (randomR (0, 3) gen) :: Int of
    0 -> Add (generateI g  $ pred d) (generateI g' $ pred d)
    1 -> Const x
    2 -> Sub (generateI  g $ pred d) (generateI g' $ pred d)
    3 -> If (generateB g $ pred d) (generateI g' $ pred d) (generateI g'' $ pred d)
    where
      x = fst $ randomR (0, 5) gen :: Int
      (g, gen') = split gen
      (g', g'') = split gen'


gen = mkStdGen 0
(g, g') = split gen
grids = generateGrids g'

i = generateI g 3



