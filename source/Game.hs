-- | This module contains functions to run matches between different implementations of ant behaviours
module Game (Game(..)
            , initGame
            , runGame
            , runMatch
            , saveGame
            , tournament
            , matchPercentage
            , matchStat
            , saveStat
            ) where

import System.Random
import Data.List
import Helper
import Grid
import Ant
import Memory

nbMove = 35
nbMatch = 1000
antNumber = 2

data Game = Game {initialGrid :: Grid
                 , grid :: Grid
                 , ants :: [Ant]
                 } 
                            
instance Show Game where
  show g = show (initialGrid g) ++ "\n" ++ show (ants g) ++ "\n"

-- | Initialise a game
initGame :: Grid -> [AntNb -> Memory -> Grid -> Direction] -> Game
initGame gr mvs = Game gr gr a
  where a = map (\ (mv, i) -> initAnt i (mv i)) $ zip mvs [0, 1]

-- | Given a game and a specific ant, update the grid by moving the ant
updateGame :: Game -> AntNb -> Game
updateGame g a = 
  if kill $ ants g !! (succ a `mod` antNumber) -- if the ant has been killed no update - works only for two ants
  then g
  else Game (initialGrid g) gr' (replaceNth a ant' (ants g))
    where
      ant = ants g !! a
      gr = grid g
      (ant', gr') = updateAnt ant gr
      
-- | To test whether a game is over or not      
endGame :: Game -> Bool      
endGame g = any (\ a -> length (directions a) >= nbMove) (ants g)
            
-- | run a game between two ants     
runGame :: Game -> Game
runGame g = runGame' g 0
  where
    runGame' g i = if endGame g 
                   then g
                   else runGame' (updateGame g i) (succ i `mod` antNumber)
-- | Winner of a game
gameWinner :: Game -> AntNb    
gameWinner g = antNb (head $ sortBy (\ a a' -> compare (Ant.score a') (Ant.score a)) (ants g)) -- TODO verify that 1 is winning in case of tie --> seems to be OK

-- | run a set of games
runMatch :: [Grid] -> [AntNb -> Memory -> Grid -> Direction] -> [Game]
runMatch gs moves = games' -- Works only for two ants
  where
    grids = take nbMatch gs
    games = map (\ (grid, b) -> initGame grid (if b then moves else reverse moves)) (zip grids (cycle [True, False])) -- to alternate the ants initial positions in order to give a 50% equity
    games' = map runGame games

-- | Percentage of victory    
matchPercentage :: [Game] -> Float
matchPercentage m = score 0 / (fromIntegral nbMatch)
  where 
    winners = map gameWinner m
    score ant = foldl (\ sum (nb, b) -> if (if b then (==) else (/=) ) nb ant then sum + 1 else sum) 0 (zip winners (cycle [True, False]))
    
-- | tournament between mutiple ants
tournament :: [Grid] -> ([AntNb -> Memory -> Grid -> Direction] -> [[AntNb -> Memory -> Grid -> Direction]]) -> [AntNb -> Memory -> Grid -> Direction] -> [[Game]]
tournament gs selection moves = map (\ (gs, ms) -> runMatch gs ms) $ zip (group' nbMatch gs) (selection moves)

-- | save a game to the filesystem
saveGame :: String -> Game -> IO ()
saveGame file g = do 
  appendFile file $ show g

matchStat :: [Game] -> String
matchStat m = stat 0 ++ " " ++ stat 1 ++ "\n" -- Currently only support two ants.
  where
    totalScore a = foldl (\ sum as -> sum + Ant.score (as !! a)) 0 (map (\ g -> ants g) m)
    totalKill a =  foldl (\ sum as -> sum + if kill (as !! a) then 1 else 0) 0 (map (\ g -> ants g) m)
    totalVictory a = foldl (\ victory ant -> if a == ant then succ victory else victory) 0 (map gameWinner m)
    stat a = show (totalVictory a) ++ " " ++ show (totalScore a) ++ " " ++ show (totalKill a)
    
-- | Save stats to a file
saveStat :: String -> [Game] -> IO ()
saveStat file m = do
  appendFile file $ matchStat m