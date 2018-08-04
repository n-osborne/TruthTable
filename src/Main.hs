{-|
Module      : Main
Description : Compute truth table of a propositional logic formula
Copyright   : (c) Nicolas Osborne, 2018
License     : BSD-3
Maintainer  : nicolas.osborne@etu.univ-lille1.fr

-}
module Main where

import Prelude
import TruthTable.WellFormedFormula
import TruthTable.PropLogTree
import TruthTable.PropLogValuation

main = do
  arg <- getArgs
  if (length arg) /= 1
    then putStrLn "Error"
    else do (map putStrLn truthtable)
              where truthtable = truthTable (buildTree (parsePL (head arg))) (truthValueArr (setOfNames (parsePL (head arg))) []) 
