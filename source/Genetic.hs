-- | This module is aimed at generate genetically programmed ants
module Genetic()
       where

import Data.List
import Grid

data A = Rect (Int, Int, Int, Int)

data B = And B B
       | Or B B
       | Not B
       | IsFood A
       | IsEnemy A
       | IsSmaller F F

data F = Const Float
       | Add F F
       | Sub F F
       | Mul F F
       | If B F F
         
evalB :: B -> Grid -> Bool
evalB (And b1 b2) g = evalB b1 g && evalB b2 g
evalB (Or b1 b2) g = evalB b1 g || evalB b2 g
evalB (Not b) g = not $ evalB b g
evalB (IsFood a) g = not $ null $ food g `intersect` rect2List a
evalB (IsEnemy a) g = if length (antPositions g) == 1 
                      then False
                      else not $ null $ antPositions g `intersect` rect2List a -- Buggy cannot differentitate other ant and itself
evalB (IsSmaller f1 f2) g = evalF f1 g < evalF f2 g

evalF :: F -> Grid -> Float
evalF (Const f) _ = f
evalF (Add f1 f2) g = evalF f1 g + evalF f2 g
evalF (Sub f1 f2) g = evalF f1 g - evalF f2 g
evalF (Mul f1 f2) g = evalF f1 g * evalF f2 g
evalF (If b f1 f2) g = if evalB b g then evalF f1 g else evalF f2 g

rect2List :: A -> [(Int, Int)]
rect2List (Rect (x, y, dx, dy)) = [(x' `mod` dimension , y' `mod` dimension) | x' <- [x..(x+dx)], y' <- [y..(y+dy)]]
