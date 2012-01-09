-- | This module is aimed at generate genetically programmed ants
module Genetic()
       where

import System.Random
import Data.List
import Grid


dxA = 4 -- maximum dx size of a Rect
dyA = 4 -- maximum dy size of a Rect

-- | Represent a rectangular arear on the grid
-- Rect (x, y, dx, dy) is a rectangle starting at position (x, y) on the grid and of length dx and dy
data A = Rect (Int, Int, Int, Int) deriving Show

-- | Represent boolean function
data B = And B B
       | Or B B
       | Not B
       | IsFood A
       | IsEnemy A
       | IsSmaller I I deriving Show

-- | Represent integer function
data I = Const Int
       | Add I I
       | Sub I I
       | Mul I I
       | If B I I deriving Show
                           
-- | evaluate a B expression                           
evalB :: B -> Grid -> Bool
evalB (And b1 b2) g = evalB b1 g && evalB b2 g
evalB (Or b1 b2) g = evalB b1 g || evalB b2 g
evalB (Not b) g = not $ evalB b g
evalB (IsFood a) g = not $ null $ food g `intersect` rect2List a
evalB (IsEnemy a) g = if length (antPositions g) == 1 
                      then False
                      else not $ null $ antPositions g `intersect` rect2List a -- Buggy cannot differentitate other ant from itself
evalB (IsSmaller f1 f2) g = evalI f1 g < evalI f2 g

-- | evaluate a F expression
evalI :: I -> Grid -> Int
evalI (Const f) _ = f
evalI (Add f1 f2) g = evalI f1 g + evalI f2 g
evalI (Sub f1 f2) g = evalI f1 g - evalI f2 g
evalI (Mul f1 f2) g = evalI f1 g * evalI f2 g
evalI (If b f1 f2) g = if evalB b g then evalI f1 g else evalI f2 g

-- | Convert a type A to the List of positions that constitute the rectangle 
rect2List :: A -> [(Int, Int)]
rect2List (Rect (x, y, dx, dy)) = [(x' `mod` dimension , y' `mod` dimension) | x' <- [x..(x+dx)], y' <- [y..(y+dy)]]

-- | given an (infinite) list of pseudo-generated integer, construct a Rect
generateA :: [Int] -> Int -> A
generateA (a:b:c:d:xs) _ = Rect (a `mod` dimension, b `mod` dimension, c `mod` dxA, d `mod` dyA)

-- | given an (infinite) list of pseudo-generated integer, construct a B expression of height h
generateB :: [Int] -> Int -> B
generateB (x:xs) h = 
  if h <= 2 
  then if x `mod` 2 == 0 then IsFood (generateA xs h) else IsEnemy (generateA xs h)
  else 
    case x `mod` 6 of
      0 -> And (generateB xs $ pred h) (generateB xs' $ pred h)
      1 -> Or (generateB xs $ pred h) (generateB xs' $ pred h)
      2 -> Not (generateB xs $ pred h)
      3 -> IsFood (generateA xs $ pred h)
      4 -> IsEnemy (generateA xs $ pred h)
      5 -> IsSmaller (generateI xs $ pred h) (generateI xs' $ pred h) 
      where xs' = drop 7 xs

-- | given an (infinite) list of pseudo-generated integer, construct a I expression of height h
generateI :: [Int] -> Int -> I
generateI (x:xs) h = 
  if h <= 1 
  then Const x
  else
    case x `mod` 8 of
      0 -> Add (generateI xs $ pred h) (generateI xs' $ pred h)
      1 -> Const x
      2 -> Sub (generateI xs $ pred h) (generateI xs' $ pred h)
      _ -> If (generateB xs $ pred h) (generateI xs' $ pred h) (generateI xs'' $ pred h)
      where xs' = drop (x * 10) xs
            xs'' = drop (x * 100) xs'

gen = mkStdGen 12348
grids = generateGrids gen
xs = randomRs (0, 10) (mkStdGen 0) :: [Int]

test = evalI (generateI xs 4) (head grids)
test' = evalI (generateI xs 4) (rotateGrid $ head grids)

