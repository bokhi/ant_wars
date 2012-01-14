-- | This module contains the data and functions required to construct ant program expressions
module Expression (I(..)
                  , IB(..)
                  , depthI
                  , selectI
                  , replaceIB
                  , generateB
                  , nodeB
                  , nodeI
                  , replaceII
                  , generateI
                  , evalI) where

import System.Random
import Data.List
import Helper
import Grid
import Memory
import Game

-- | (x, y, dx, dy) is a rectangle starting at position (x, y) on the grid and of length dx and dy
type Rect = (Int, Int, Int, Int)

dx = 4 -- maximum dx size of a rectangle
dy = 4 -- maximum dy size of a rectangle

-- | express which part of the grammar are used to construct B and I expressions - ranging from (0, 0) to (6, 10)
expressivenessLevel = (6, 10)

-- | Represent boolean function
data B = IsFood Rect
       | IsEnemy Rect
       | And B B
       | Or B B
       | Not B
       | IsSmaller I I 
       | IsEqual I I 
       deriving (Show, Read)

-- | Represent integer function
data I = Const Int
       | If B I I 
       | Add I I
       | Sub I I
       | Mul I I
       | NbFood Rect
       | NbEmpty Rect
       | NbVisited Rect
       | Point
       | PointLeft
       | TimeLeft
       deriving (Show, Read)
                           
-- | number of nodes in a B expression
nodeB :: B -> Int
nodeB b = case b of
  IsFood _ -> 1
  IsEnemy _ -> 1
  And b1 b2 -> succ $ nodeB b1 + nodeB b2
  Or b1 b2 -> succ $ nodeB b1 + nodeB b2
  Not b' -> succ $ nodeB b'
  IsSmaller i1 i2 -> succ $ nodeI i1 + nodeI i2
  IsEqual i1 i2 -> succ $ nodeI i1 + nodeI i2
  
  
-- | number of nodes in a B expression  
nodeI :: I -> Int
nodeI i = case i of
  Const _ -> 1
  If b i1 i2 -> succ $ nodeB b + nodeI i1 + nodeI i2
  Add i1 i2 -> succ $ nodeI i1 + nodeI i2
  Sub i1 i2 -> succ $ nodeI i1 + nodeI i2
  Mul i1 i2 -> succ $ nodeI i1 + nodeI i2
  NbFood _ -> 1
  NbEmpty _ -> 1
  NbVisited _ -> 1
  Point -> 1
  PointLeft -> 1
  TimeLeft -> 1
  
-- | depth of a B expression
depthB :: B -> Int
depthB b = case b of
  IsFood _ -> 1
  IsEnemy _ -> 1
  And b1 b2 -> succ $ depthB b1 `max` depthB b2
  Or b1 b2 -> succ $ depthB b1 `max` depthB b2
  Not b' -> succ $ depthB b'
  IsSmaller i1 i2 -> succ $ depthI i1 `max` depthI i2
  IsEqual i1 i2 -> succ $ depthI i1 `max` depthI i2
  
-- | depth of a I expression
depthI :: I -> Int  
depthI i = case i of
  Const _ -> 1
  If b i1 i2 -> succ $ depthB b `max` depthI i1 `max` depthI i2
  Add i1 i2 -> succ $ depthI i1 `max` depthI i2
  Sub i1 i2 -> succ $ depthI i1 `max` depthI i2
  Mul i1 i2 -> succ $ depthI i1 `max` depthI i2
  NbFood _ -> 1
  NbEmpty _ -> 1
  NbVisited _ -> 1
  Point -> 1
  PointLeft -> 1
  TimeLeft -> 1
      
-- | given a seed, construct a B expression of depth d
generateB :: StdGen -> Int -> B            
generateB gen depth = 
  if depth <= 1
  then (case fst (randomR (0, min 1 (fst expressivenessLevel)) (g !! 0)) :: Int of
           0 -> IsFood rect 
           1 -> IsEnemy rect)
  else (case fst (randomR (0, min 6 (fst expressivenessLevel)) (g !! 0)) :: Int of -- what part of the grammar to generate
           0 -> IsFood rect
           1 -> IsEnemy rect
           2 -> And (generateB (g !! 2) $ pred depth) (generateB (g !! 3) $ pred depth)
           3 -> Or (generateB (g !! 2) $ pred depth) (generateB (g !! 3) $ pred depth)
           4 -> Not (generateB (g !! 2) $ pred depth)
           5 -> IsSmaller (generateI (g !! 2) (pred depth)) (generateI (g !! 3) (pred depth))
           6 -> IsEqual (generateI (g !! 2) (pred depth)) (generateI (g !! 3) (pred depth)))
    where 
      g = splits 4 gen
      (a:b:c:d:xs) = randoms (g !! 1) :: [Int]
      rect = (a `mod` dimension, b `mod` dimension, c `mod` dx, d `mod` dy)

-- | given a seed, construct a I expression of depth d
generateI :: StdGen -> Int -> I
generateI gen depth = 
  if depth <= 1 
  then case fst (randomR (0, max 0 $ min 6 (snd expressivenessLevel - 4)) (g !! 0)) :: Int of  
    0 -> Const x
    1 -> NbFood rect
    2 -> NbEmpty rect
    3 -> NbVisited rect
    4 -> Point
    5 -> PointLeft
    6 -> TimeLeft
  else case fst (randomR (0, min 10 (snd expressivenessLevel)) (g !! 0)) :: Int of
    0 -> Const x
    1 -> If (generateB (g !! 3) (pred depth)) (generateI (g !! 4) (pred depth)) (generateI (g !! 5) (pred depth))
    2 -> Add (generateI (g !! 3)  $ pred depth) (generateI (g !! 4) $ pred depth)
    3 -> Sub (generateI (g !! 3) $ pred depth) (generateI (g !! 4) $ pred depth)
    4 -> Mul (generateI (g !! 3) $ pred depth) (generateI (g !! 4) $ pred depth)
    5 -> NbFood rect
    6 -> NbEmpty rect
    7 -> NbVisited rect
    8 -> Point
    9 -> PointLeft
    10 -> TimeLeft
    where
      g = splits 6 gen
      x = fst $ randomR (0, 5) (g !! 1) :: Int
      (a:b:c:d:xs) = randoms (g !! 2) :: [Int]
      rect = (a `mod` dimension, b `mod` dimension, c `mod` dx, d `mod` dy)
                
-- | replace b2 in b1 at position pos                
replaceBB :: Int -> B -> B -> Int -> B  
replaceBB pos b1 b2 n 
  | pos == n = b2
  | otherwise = case b1 of
    And b1' b2' -> if pos < succ n + nodeB b1' then And (replaceBB pos b1' b2 (succ n)) b2' else And b1' (replaceBB pos b2' b2 (succ n + nodeB b1'))
    Or b1' b2' -> if pos < succ n + nodeB b1' then Or (replaceBB pos b1' b2 (succ n)) b2' else Or b1' (replaceBB pos b2' b2 (succ n + nodeB b1'))
    Not b -> Not (replaceBB pos b b2 (succ n))
    IsSmaller i1 i2 -> if pos < succ n + nodeI i1 then IsSmaller (replaceIB pos i1 b2 (succ n)) i2 else IsSmaller i1 (replaceIB pos i2 b2 (succ n + nodeI i1))
    IsEqual i1 i2 -> if pos < succ n + nodeI i1 then IsEqual (replaceIB pos i1 b2 (succ n)) i2 else IsEqual i1 (replaceIB pos i2 b2 (succ n + nodeI i1))

-- | replace i in b at position pos                
replaceBI :: Int -> B -> I -> Int -> B  
replaceBI pos b i n = case b of
  And b1 b2 -> if pos < succ n + nodeB b1 then And (replaceBI pos b1 i (succ n)) b2 else And b1 (replaceBI pos b2 i (succ n + nodeB b1))
  Or b1 b2 -> if pos < succ n + nodeB b1 then Or (replaceBI pos b1 i (succ n)) b2 else Or b1 (replaceBI pos b2 i (succ n + nodeB b1))
  Not b' -> Not (replaceBI pos b' i (succ n))
  IsSmaller i1 i2 -> if pos < succ n + nodeI i1 then IsSmaller (replaceII pos i1 i (succ n)) i2 else IsSmaller i1 (replaceII pos i2 i (succ n + nodeI i1))
  IsEqual i1 i2 -> if pos < succ n + nodeI i1 then IsEqual (replaceII pos i1 i (succ n)) i2 else IsEqual i1 (replaceII pos i2 i (succ n + nodeI i1))

-- | replace i2 in i1 at position pos                            
replaceII :: Int -> I -> I -> Int -> I                           
replaceII pos i1 i2 n
  | pos == n = i2
  | otherwise = case i1 of
    If b i1' i2' -> if pos < succ n + nodeB b 
                    then If (replaceBI pos b i2 (succ n)) i1' i2'
                    else if pos < succ n + nodeB b + nodeI i1'
                         then If b (replaceII pos i1' i2 (succ n + nodeB b)) i2'
                         else If b i1' (replaceII pos i2' i2 (succ n + nodeB b + nodeI i1'))
    Add i1' i2' -> if pos < succ n + nodeI i1' then Add (replaceII pos i1' i2 (succ n)) i2' else Add i1' (replaceII pos i2' i2 (succ n + nodeI i1'))
    Sub i1' i2' -> if pos < succ n + nodeI i1' then Sub (replaceII pos i1' i2 (succ n)) i2' else Sub i1' (replaceII pos i2' i2 (succ n + nodeI i1'))
    Mul i1' i2' -> if pos < succ n + nodeI i1' then Mul (replaceII pos i1' i2 (succ n)) i2' else Mul i1' (replaceII pos i2' i2 (succ n + nodeI i1'))
        
-- | replace b in i at position pos        
replaceIB :: Int -> I -> B -> Int -> I
replaceIB pos i b n = case i of
  If b' i1 i2 -> if pos < succ n + nodeB b'
                 then If (replaceBB pos b' b (succ n)) i1 i2
                 else if pos < succ n + nodeB b' + nodeI i1 
                      then If b (replaceIB pos i1 b (succ n + nodeB b')) i2
                      else If b i1 (replaceIB pos i2 b (succ n + nodeB b' + nodeI i1))
  Add i1 i2 -> if pos < succ n + nodeI i1 then Add (replaceIB pos i1 b (succ n)) i2 else Add i1 (replaceIB pos i2 b (succ n + nodeI i1))
  Sub i1 i2 -> if pos < succ n + nodeI i1 then Sub (replaceIB pos i1 b (succ n)) i2 else Sub i1 (replaceIB pos i2 b (succ n + nodeI i1))
  Mul i1 i2 -> if pos < succ n + nodeI i1 then Mul (replaceIB pos i1 b (succ n)) i2 else Mul i1 (replaceIB pos i2 b (succ n + nodeI i1))
                           
-- | Convert a type A to the List of positions that constitute the rectangle 
rect2List :: (Int, Int) -> Rect -> [(Int, Int)]
rect2List (ax, ay) (x, y, dx, dy) = [(x' `mod` dimension , y' `mod` dimension) | x' <- [(ax+x)..(ax+x+dx)], y' <- [(ay+y)..(ay+y+dy)]] -- x and y are related to the ant position

-- | evaluate a B expression                           
evalB :: B -> AntNb -> Memory -> Grid -> Bool
evalB (IsFood r) a _ g = not $ null $ food g `intersect` rect2List (antPosition g a) r
evalB (IsEnemy r) a _ g = if length (antPositions g) == 1 
                          then False
                          else elem (antPositions g !! (succ a `mod` 2)) $ rect2List (antPosition g a) r
evalB (And b1 b2) a m g = evalB b1 a m g && evalB b2 a m g
evalB (Or b1 b2) a m g = evalB b1 a m g || evalB b2 a m g
evalB (Not b) a m g = not $ evalB b a m g
evalB (IsSmaller f1 f2) a m g = evalI f1 a m g < evalI f2 a m g
evalB (IsEqual f1 f2) a m g = evalI f1 a m g == evalI f2 a m g

-- | evaluate a F expression
evalI :: I -> AntNb -> Memory -> Grid -> Int
evalI (Const f) a m g = f
evalI (If b f1 f2) a m g = if evalB b a m g then evalI f1 a m g else evalI f2 a m g
evalI (Add f1 f2) a m g = evalI f1 a m g + evalI f2 a m g
evalI (Sub f1 f2) a m g = evalI f1 a m g - evalI f2 a m g
evalI (Mul f1 f2) a m g = evalI f1 a m g * evalI f2 a m g
evalI (NbFood r) a _ g = length $ food g `intersect` rect2List (antPosition g a) r
evalI (NbEmpty r) a m g = length (rect2List (antPosition g a) r) - (evalI (NbFood r) a m g + if evalB (IsEnemy r) a m g then 1 else 0)
evalI (NbVisited r) a m g = length $ tracks m `intersect` rect2List (antPosition g a) r
evalI (Point) a m g = (score g) !! a
evalI (PointLeft) a m g = nbFood - evalI (Point) a m g
evalI (TimeLeft) a m g = nbMove - length (tracks m)

-- | data of type I or B use when the returned node's type is unknown
data IB = B' B | I' I deriving Show

-- | return the pos-nth node from a B expression
selectB :: Int -> B -> Int -> IB
selectB pos b n 
  | pos == n = B' b
  | otherwise = case b of
    And b1 b2 -> if pos < succ n + nodeB b1 then selectB pos b1 (succ n) else selectB pos b2 (succ n + nodeB b1)
    Or b1 b2 -> if pos < succ n + nodeB b1 then selectB pos b1 (succ n) else selectB pos b2 (succ n + nodeB b1)
    Not b' -> selectB pos b' $ succ n
    IsSmaller i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
    IsEqual i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
    
-- | return the pos-nth node from a I expression
selectI :: Int -> I -> Int -> IB
selectI pos i n
  | pos == n = I' i
  | otherwise = case i of
    If b i1 i2 -> if pos < succ n + nodeB b 
                  then selectB pos b (succ n) 
                  else if pos < succ n + nodeB b + nodeI i1 
                       then selectI pos i1 (succ n + nodeB b)
                       else selectI pos i2 (succ n + nodeB b + nodeI i1)
    Add i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
    Sub i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
    Mul i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
