{-|
Module      : Main
Description : Compute truth table of a propositional logic formula
Copyright   : (c) Nicolas Osborne, 2018
License     : BSD-3
Maintainer  : nicolas.osborne@etu.univ-lille1.fr

-}
module Main where

import System.Environment
import Data.Maybe
import TruthTable.WellFormedFormula
import TruthTable.PropLogTree
import TruthTable.PropLogValuation

main = do args <- getArgs
          if (length args) /= 1
            then putStrLn "Error"
            else do
            let prop = head args
            let prop' = parsePL prop
            if prop' == Nothing
              then putStrLn "Error"
              else do
              let wff = (fromMaybe [] prop') 
              let tree = buildTree wff
              let setBool = truthValuesArr (setOfNames wff) []
              let tt = truthTable tree setBool
              mapM_ print tt

