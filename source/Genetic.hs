-- | this module is aimed at generate genetically programmed ants
module Genetic (I(..)
                ) where


import Debug.Trace
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

-- | Represent boolean function
data B = And B B
       | Or B B
       | Not B
       | IsFood Rect
       | IsEnemy Rect
       | IsSmaller I I deriving (Show, Read)

-- | Represent integer function
data I = Const Int
       | Add I I
       | Sub I I
       | Mul I I
       | If B I I deriving (Show, Read)
                           
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
    
-- | given a seed, construct a B expression of depth d
generateB :: StdGen -> Int -> B            
generateB gen depth = 
  if depth <= 1
  then (if fst (random g ) :: Bool then IsFood rect else IsEnemy rect)
  else (case k of 
    0 -> And (generateB g $ pred depth) (generateB g' $ pred depth)
    1 -> Or (generateB g $ pred depth) (generateB g' $ pred depth)
    2 -> Not (generateB g $ pred depth)
    3 -> IsFood rect
    4 -> IsEnemy rect
    5 -> IsSmaller (generateI g (pred depth)) (generateI g' (pred depth)))
    where 
      (k, gen') = randomR (0, 5) gen :: (Int, StdGen)
      (g, g') = split gen'
      (a:b:c:d:xs) = randoms g :: [Int]
      rect = (a `mod` dimension, b `mod` dimension, c `mod` dx, d `mod` dy)


-- | given a seed, construct a I expression of depth d
generateI :: StdGen -> Int -> I
generateI gen d = 
  if d <= 1 
  then Const (fst $ randomR (0, 5) gen :: Int)
  else case fst (randomR (0, 3) gen) :: Int of
    0 -> Add (generateI g  $ pred d) (generateI g' $ pred d)
    1 -> Const x
    2 -> Sub (generateI  g $ pred d) (generateI g' $ pred d)
    3 -> If (generateB g (pred d)) (generateI g' (pred d)) (generateI g'' (pred d))
    where
      x = fst $ randomR (0, 5) gen :: Int
      (g, gen') = split gen
      (g', g'') = split gen'
                
-- | replace b2 in b1 at position pos                
replaceBB :: Int -> B -> B -> Int -> B  
replaceBB pos b1 b2 n 
  | pos == n = b2
  | otherwise = case b1 of
    And b1' b2' -> if pos < succ n + nodeB b1 then And (replaceBB pos b1' b2 (succ n)) b2' else And b1' (replaceBB pos b2' b2 (succ n + nodeB b1'))
    Or b1' b2' -> if pos < succ n + nodeB b1 then Or (replaceBB pos b1' b2 (succ n)) b2' else Or b1' (replaceBB pos b2' b2 (succ n + nodeB b1'))
    Not b -> Not (replaceBB pos b b2 (succ n))
    IsSmaller i1 i2 -> if pos < succ n + nodeI i1 then IsSmaller (replaceIB pos i1 b2 (succ n)) i2 else IsSmaller i1 (replaceIB pos i2 b2 (succ n + nodeI i1))

-- | replace i in b at position pos                
replaceBI :: Int -> B -> I -> Int -> B  
replaceBI pos b i n = case b of
  And b1 b2 -> if pos < succ n + nodeB b1 then And (replaceBI pos b1 i (succ n)) b2 else And b1 (replaceBI pos b2 i (succ n + nodeB b1))
  Or b1 b2 -> if pos < succ n + nodeB b1 then Or (replaceBI pos b1 i (succ n)) b2 else Or b1 (replaceBI pos b2 i (succ n + nodeB b1))
  Not b' -> Not (replaceBI pos b' i (succ n))
  IsSmaller i1 i2 -> if pos < succ n + nodeI i1 then IsSmaller (replaceII pos i1 i (succ n)) i2 else IsSmaller i1 (replaceII pos i2 i (succ n + nodeI i1))
                                                      

-- | replace i2 in i1 at position pos                            
replaceII :: Int -> I -> I -> Int -> I                           
replaceII pos i1 i2 n
  | pos == n = i2
  | otherwise = case i1 of
    Add i1' i2' -> if pos < succ n + nodeI i1' then Add (replaceII pos i1' i2 (succ n)) i2' else Add i1' (replaceII pos i2' i2 (succ n + nodeI i1'))
    Sub i1' i2' -> if pos < succ n + nodeI i1' then Sub (replaceII pos i1' i2 (succ n)) i2' else Sub i1' (replaceII pos i2' i2 (succ n + nodeI i1'))
    Mul i1' i2' -> if pos < succ n + nodeI i1' then Mul (replaceII pos i1' i2 (succ n)) i2' else Mul i1' (replaceII pos i2' i2 (succ n + nodeI i1'))
    If b i1' i2' -> if pos < succ n + nodeB b 
                    then If (replaceBI pos b i2 (succ n)) i1' i2'
                    else if pos < succ n + nodeB b + nodeI i1'
                         then If b (replaceII pos i1' i2 (succ n + nodeB b)) i2'
                         else If b i1' (replaceII pos i2' i2 (succ n + nodeB b + nodeI i1))
        
-- | replace b in i at position pos        
replaceIB :: Int -> I -> B -> Int -> I
replaceIB pos i b n = case i of
  Add i1 i2 -> if pos < succ n + nodeI i1 then Add (replaceIB pos i1 b (succ n)) i2 else Add i1 (replaceIB pos i2 b (succ n + nodeI i1))
  Sub i1 i2 -> if pos < succ n + nodeI i1 then Sub (replaceIB pos i1 b (succ n)) i2 else Sub i1 (replaceIB pos i2 b (succ n + nodeI i1))
  Mul i1 i2 -> if pos < succ n + nodeI i1 then Mul (replaceIB pos i1 b (succ n)) i2 else Mul i1 (replaceIB pos i2 b (succ n + nodeI i1))
  If b' i1 i2 -> if pos < succ n + nodeB b 
                 then If (replaceBB pos b' b (succ n)) i1 i2
                 else if pos < succ n + nodeB b + nodeI i1 
                      then If b (replaceIB pos i1 b (succ n + nodeB b')) i2
                      else If b i1 (replaceIB pos i2 b (succ n + nodeB b' + nodeI i1))
                           
-- | Convert a type A to the List of positions that constitute the rectangle 
rect2List :: Rect -> [(Int, Int)]
rect2List (x, y, dx, dy) = [(x' `mod` dimension , y' `mod` dimension) | x' <- [x..(x+dx)], y' <- [y..(y+dy)]]

-- | evaluate a B expression                           
evalB :: B -> AntNb -> Memory -> Grid -> Bool
evalB (And b1 b2) a m g = evalB b1 a m g && evalB b2 a m g
evalB (Or b1 b2) a m g = evalB b1 a m g || evalB b2 a m g
evalB (Not b) a m g = not $ evalB b a m g
evalB (IsFood r) _ m g = not $ null $ food g `intersect` rect2List r
evalB (IsEnemy r) a m g = if length (antPositions g) == 1 
                          then False
                          else elem (antPositions g !! (succ a `mod` 2)) $ rect2List r
evalB (IsSmaller f1 f2) a m g = evalI f1 a m g < evalI f2 a m g

-- | evaluate a F expression
evalI :: I -> AntNb -> Memory -> Grid -> Int
evalI (Const f) a m g = f
evalI (Add f1 f2) a m g = evalI f1 a m g + evalI f2 a m g
evalI (Sub f1 f2) a m g = evalI f1 a m g - evalI f2 a m g
evalI (Mul f1 f2) a m g = evalI f1 a m g * evalI f2 a m g
evalI (If b f1 f2) a m g = if evalB b a m g then evalI f1 a m g else evalI f2 a m g


-- | data of type I or B
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
    
-- | return the pos-nth node from a I expression
selectI :: Int -> I -> Int -> IB
selectI pos i n
  | pos == n = I' i
  | otherwise = case i of
    Add i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
    Sub i1 i2 -> if pos < succ n + nodeI i1 then selectI pos i1 (succ n) else selectI pos i2 (succ n + nodeI i1)
    If b i1 i2 -> if pos < succ n + nodeB b 
                  then selectB pos b (succ n) 
                  else if pos < succ n + nodeB b + nodeI i1 
                       then selectI pos i1 (succ n + nodeB b)
                       else selectI pos i2 (succ n + nodeB b + nodeI i1)
                            
-- | crossing-over between i1 and i2
cross :: StdGen -> I -> I -> (I, I)                            
cross gen i1 i2 = (i1', i2')
  where
    (g, g') = split gen
    pos1 = fst (randomR (1, nodeI i1) g) :: Int
    pos2 = fst (randomR (1, nodeI i2) g') :: Int
    i1' = case selectI pos2 i2 1 of
      B' b -> replaceIB pos1 i1 b 1
      I' i -> replaceII pos1 i1 i 1
    i2' = case selectI pos1 i1 1 of
      B' b -> replaceIB pos2 i2 b 1
      I' i -> replaceII pos2 i2 i 1
      
-- | mutate a I expression      
mutate :: StdGen -> I -> I      
mutate gen i = case selectI pos i 1 of
  B' b -> replaceIB pos i (generateB g' (nodeB b)) 1
  I' i' -> replaceII pos i (generateI g' (nodeI i')) 1
  where
    (g, g') = split gen
    pos = fst (randomR (1, nodeI i) g) :: Int
    
-- | generate a population and genetic programs
generatePop :: StdGen -> Int -> Int -> [(I, I)]    
generatePop _ 0 _ = []
generatePop gen n d = (i, i') : generatePop g'' (pred n) d
  where
    (g, gen') = split gen
    (g', g'') = split gen'
    i = generateI g d
    i' = generateI g' d
    
-- | select n individuals from a given population    
selectIndividual :: StdGen -> Int -> [(I, I)] -> [(I, I)]    
selectIndividual _ 0 _ = []
selectIndividual gen n p = i : selectIndividual g' (pred n) p'
  where 
    (g, g') = split gen
    (i, p') = removeNth (fst (randomR (0, pred $ length p) g) :: Int) p

-- | run a tournament to select an individual
selection :: StdGen -> [(I, I)] -> [(I, I)]
selection gen [] = []
selection gen [x] = [x]
selection gen (x:x':xs) = (if winner >= 0.5 then x else x') : selection g' xs
  where 
    (g, g') = split gen 
    grids = generateGrids g
    winner = matchPercentage $ runMatch grids [geneticAnt x, geneticAnt x']
    
-- | best individual of the selection process    
selected :: StdGen -> [(I, I)] -> (I, I)    
selected _ [x] = x
selected gen xs = selected g (selection g' xs)
  where
    (g, g') = split gen
    



-- | The I expression trees are used to evaluate the result of N and NE movement on the grid 
-- rotation properties of the problem are thus exploited
geneticAnt :: (I, I) -> AntNb -> Memory -> Grid -> Direction 
geneticAnt (t, t') a m g = d
  where
    g' = rotateGrid g
    g'' = rotateGrid g'
    g''' = rotateGrid g''
    gs = [g, g', g'', g''']
    ds = [N, W, S, E, NE, NW, SW, SE]
    es = map (evalI t a m) gs
    es' = map (evalI t' a m) gs
    (_, d) = maximumBy (\ x x' -> compare (fst x) (fst x')) (zip (es ++ es') ds) -- the greatest number gives us the direction to go to 





