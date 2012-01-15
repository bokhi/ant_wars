-- | This module contains functions to run matches between different ants
module Game (Game(..)
            , nbMove
            , nbMatch
            , initGame
            , runGame
            , runMatch
            , saveGame
            , tournament
            , matchPercentage
            , matchStat
            , genAntStat
            , saveStat
            ) where

import System.Random
import Data.List
import Helper
import Grid
import Ant
import Memory

nbMove = 35 -- number of mooves by player by game
nbMatch = 6 -- number of games in a match
antNumber = 2 -- number of ant playing a game

data Game = Game {initialGrid :: Grid -- to keep trace of the initial grid 
                 , grid :: Grid -- the current state of the game
                 , ants :: [Ant] -- the ants that are playing the game
                 } 
                            
-- | Represent the function caracterising the ant algorithm            
type AntMove = AntNb -> Memory -> Grid -> Direction

instance Show Game where
  show g = show (initialGrid g) ++ "\n" ++ show (ants g) ++ "\n"

-- | Initialise a game
initGame :: Grid -> [AntMove] -> Game
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
runGame g = runGame' g 0 (nbMove * antNumber) -- the ant0 starts playing
  where
    runGame' g i n 
      | n == 0 = g
      | otherwise = runGame' (updateGame g i) (succ i `mod` antNumber) (pred n)
                        
-- | Winner of a game
gameWinner :: Game -> AntNb    
gameWinner g = if score0 >= score1 then 0 else 1 -- ant0 wins in case of tie
  where
    [score0, score1] = Grid.score $ grid g

-- | run a set of games
runMatch :: [Grid] -> [AntMove] -> [Game]
runMatch gs moves = games' -- Works only for two ants
  where
    grids = take nbMatch gs
    games = map (\ (grid, b) -> initGame grid (if b then moves else reverse moves)) (zip grids (cycle [True, False])) -- to alternate the ants initial positions in order to give a 50% equity
    games' = map runGame games

-- | Percentage of victory for ant0 against ant1 
matchPercentage :: [Game] -> Float
matchPercentage m = score 0 / (fromIntegral nbMatch)
  where 
    winners = map gameWinner m
    score ant = sum $ map (\ (nb, b) -> if (if b then (==) else (/=) ) nb ant then 1 else 0)  (zip winners (cycle [True, False])) -- due to the games' ant position alterning
    
-- | tournament between mutiple ants
tournament :: [Grid] -> ([AntMove] -> [[AntMove]]) -> [AntMove] -> [[Game]]
tournament gs selection moves = map (\ (gs, ms) -> runMatch gs ms) $ zip (group' nbMatch gs) (selection moves)

-- | save a game to the filesystem
saveGame :: String -> Game -> IO ()
saveGame file g = do 
  appendFile file $ show g

matchStat :: [Game] -> String
matchStat m = stat 0 ++ " " ++ stat 1 ++ "\n" 
  where
    totalScore a = sum $ map (\ g -> score (grid g) !! a) m
    totalKill a = sum $ map (\ g -> if kill (ants g !! a) then 1 else 0) m
    totalVictory a = foldl (\ victory ant -> if a == ant then succ victory else victory) 0 (map gameWinner m)
    stat a = show (totalVictory a) ++ " " ++ show (totalScore a) ++ " " ++ show (totalKill a)
    
-- | statistics from selection tournament run between genetic ants    
genAntStat :: [Game] -> (Int, Int)
genAntStat m = (totalScore, totalKill)
  where
    totalScore = sum $ map (\ g -> score (grid g) !! 0 + score (grid g) !! 1) m
    totalKill = sum $ map (\ g -> (if kill (ants g !! 0) then 1 else 0) + (if kill (ants g !! 1) then 1 else 0)) m    
-- | Save stats to a file
saveStat :: String -> [Game] -> IO ()
saveStat file m = do
  appendFile file $ matchStat m