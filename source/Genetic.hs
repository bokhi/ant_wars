-- | This module is aimed at generate genetically programmed ants
module Genetic()
       where

import System.Random
import Data.List
import Grid


dxA = 3 -- maximum dx size of a Rect
dyA = 3 -- maximum dy size of a Rect

-- | Represent a rectangular arear on the grid
-- Rect (x, y, dx, dy) is a rectangle starting at position (x, y) on the grid and of length dx and dy
data A = Rect (Int, Int, Int, Int) deriving Show

-- | Represent boolean function
data B = And B B
       | Or B B
       | Not B
       | IsFood A
       | IsEnemy A
       | IsSmaller F F deriving Show

-- | Represent floating function
data F = Const Float
       | Add F F
       | Sub F F
       | Mul F F
       | If B F F deriving Show
         
-- | evaluate a B expression                           
evalB :: B -> Grid -> Bool
evalB (And b1 b2) g = evalB b1 g && evalB b2 g
evalB (Or b1 b2) g = evalB b1 g || evalB b2 g
evalB (Not b) g = not $ evalB b g
evalB (IsFood a) g = not $ null $ food g `intersect` rect2List a
evalB (IsEnemy a) g = if length (antPositions g) == 1 
                      then False
                      else not $ null $ antPositions g `intersect` rect2List a -- Buggy cannot differentitate other ant from itself
evalB (IsSmaller f1 f2) g = evalF f1 g < evalF f2 g

-- | evaluate a F expression
evalF :: F -> Grid -> Float
evalF (Const f) _ = f
evalF (Add f1 f2) g = evalF f1 g + evalF f2 g
evalF (Sub f1 f2) g = evalF f1 g - evalF f2 g
evalF (Mul f1 f2) g = evalF f1 g * evalF f2 g
evalF (If b f1 f2) g = if evalB b g then evalF f1 g else evalF f2 g

-- | Convert a type A to the List of positions that constitute the rectangle 
rect2List :: A -> [(Int, Int)]
rect2List (Rect (x, y, dx, dy)) = [(x' `mod` dimension , y' `mod` dimension) | x' <- [x..(x+dx)], y' <- [y..(y+dy)]]

-- | given an (infinite) list of pseudo-generated integer, construct a Rect
generateA :: [Int] -> Int -> A
generateA (a:b:c:d:xs) _ = Rect (a `mod` dimension, b `mod` dimension, c `mod` dxA, d `mod` dyA)

-- | given an (infinite) list of pseudo-generated integer, construct a B expression of height h
generateB :: [Int] -> Int -> B
generateB = undefined

-- | given an (infinite) list of pseudo-generated integer, construct a F expression of height h
generateF :: [Int] -> Int -> F
generateF = undefined