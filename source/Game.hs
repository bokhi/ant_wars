-- | This module contains functions to run matches between different implementations of ant behaviours
module Game (Game(..)
            , initGame
            , runGame
            , runMatch
            , saveGame
            , tournament
            , saveStat
            ) where

import System.Random
import Data.List
import Helper
import Grid
import Ant

nbMove = 35
nbMatch = 5
nbVictory = 3

data Game = Game {initialGrid :: Grid
                 , grid :: Grid
                 , ants :: [Ant]
                 } 
                            
instance Show Game where
  show g = show (initialGrid g) ++ "\n" ++ show (ants g) ++ "\n"

-- | Initialise a game
initGame :: Grid -> [Grid -> Direction] -> Game
initGame gr mvs = Game gr gr a
  where a = map (\ (mv, i) -> initAnt i mv) $ zip mvs [0..]

-- | Given a game and a specific ant, update the grid by moving the ant
updateGame :: Game -> AntNb -> Game
updateGame g a = 
  if kill $ ants g !! ((a + 1) `mod` antNumber) -- if the ant has been killed no update - works only for two ants
  then g
  else Game (initialGrid g) gr' (replaceNth a ant' (ants g))
    where
      ant = ants g !! a
      gr = grid g
      (ant', gr') = updateAnt ant gr
    

      
-- | To test whether a game is over or not      
endGame :: Game -> Bool      
endGame g = any (\ a -> length (directions a) >= nbMove) (ants g)
            
-- | Winner of a game
gameWinner :: Game -> AntNb    
gameWinner g = antNb (head $ sortBy (\ a a' -> compare (score a') (score a)) (ants g)) -- TODO verify that 1 is winning in case of tie --> seems to be OK

-- | winner of a match
matchWinner :: [Game] -> AntNb
matchWinner m = if score 1 >= nbVictory then 1 else 2
  where
    winners = map gameWinner m
    score ant = foldl (\ sum nb -> if nb == ant then sum + 1 else sum) 0 winners

-- | run a game between two ants     
runGame :: Game -> Game
runGame g = runGame' g 0
  where
    runGame' g i = if endGame g 
                   then g
                   else runGame' (updateGame g i) (succ i `mod` antNumber)

-- | run a set of games
runMatch :: StdGen -> [Grid -> Direction] -> (AntNb, [Game])
runMatch gen moves = (matchWinner games', games') -- Works only for two ants
  where
    grids = take nbMatch (generateGrids gen)
    games = map (\ grid -> initGame grid moves) grids 
    games' = map runGame games

-- | tournament between mutiple ants
tournament = undefined

-- | save a game to the filesystem
saveGame :: String -> Game -> IO ()
saveGame file g = do 
  appendFile file $ show g

-- | Statistics extracted from a game
gameStat :: Game -> String
gameStat = undefined

matchStat :: [Game] -> String
matchStat m = show (matchWinner m) ++ " " ++ stat 1 ++ " " ++ stat 2 ++ "\n" -- Currently only support two ants.
  where
    totalScore a = foldl (\ sum as -> sum + score (as !! (pred a `mod` antNumber))) 0 (map (\ g -> ants g) m)
    totalKill a =  foldl (\ sum as -> sum + if kill (as !! (pred a `mod` antNumber)) then 1 else 0) 0 (map (\ g -> ants g) m)
    totalVictory a = foldl (\ victory ant -> if a == ant then succ victory else victory) 0 (map gameWinner m)
    stat a = show (totalVictory a) ++ " " ++ show (totalScore a) ++ " " ++ show (totalKill a)
    
-- | Save stats to a file
saveStat :: String -> [Game] -> IO ()
saveStat file m = do
  appendFile file $ matchStat m
