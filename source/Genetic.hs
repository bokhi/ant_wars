{-# OPTIONS_GHC -XDeriveDataTypeable #-} 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
-- | This module is aimed at generate genetically programmed ants
module Genetic()
       where

import GenProg
import Data.Generics
import Control.Monad
import Control.Monad.Random

data E = Plus E E
       | Minus E E
       | Times E E
       | Const Int
       deriving (Typeable,Data,Eq,Show)
                                                    
eval :: E ->  Int                
eval (Const c)     =  c
eval (Plus e1 e2)  =  (+) (eval e1) (eval e2)
eval (Minus e1 e2) =  (-) (eval e1) (eval e2)
eval (Times e1 e2) =  (*) (eval e1) (eval e2)
                         
instance GenProg (Rand StdGen) E where                         
  terminal    =  liftM Const $  getRandomR (1,9)
  nonterminal = do
    r <- getRandomR (0,2)
    [ liftM2 Plus terminal terminal,
      liftM2 Minus terminal terminal,
      liftM2 Times terminal terminal] !! r
      
myFitness :: Int -> E -> Double      -- modif
myFitness n e = error + size
  where error = realToFrac  (abs  (n - (eval e)))
        size  = (realToFrac $ nodes e) / 100
                        

params = defaultEvolParams { fitness = myFitness 12345 }
g = mkStdGen 0
i = cachedBest $ evalRand (evolve params) g