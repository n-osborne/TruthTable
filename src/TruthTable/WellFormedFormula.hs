{-|
Module      : TruthTable.WellFormedFormula
Description : This module provides the functions to transform a String into a 
              list of Token that is Maybe a Well Formed Formula of Propositional Logic
Copyright   : (c) Nicolas Osborne, 2018
Licence     : GPL-3
Maintainer  : nicolas.osborne@etu.univ-lille1.fr

This module parse a 'List' of 'Char' and output a 'List' of 'Token' in a 'Maybe' Functor.
The output if 'Just' the 'List' of 'Token' if this list corresponds to a well formed formula
(sometimes refered hereafter as wff) for the propositional logic.

== First step, parsing the 'List' of 'Char' into a 'List' of 'String'

This step is taken care of by the 'split' function.

There are three important informations:

* the separator is the whitespace
* several whitespaces following each other are considered as one separator
* parenthesis does not need whitespaces to be separated

== Second step, turn the 'List' of 'String' into a 'List' of 'Sign'

This second step is taken care of by the function 'wff'.

The 'split' function iterates through the 'List' of 'String' turning each 'String' into
the corresponding 'Sign' if it can (in case of failure, the output is 'Nothing').

Then it check whether the resulting 'List' of 'Sign's correspond to a well parenthesized
formula (checked by the 'wellParPred' function) and to a correct succession of logical
constants and logical operators (checked by the 'wellFormedPred' function).

If the 'List' of 'Sign's verifies the two predicates, then the result is 'Just' that. Otherwise,
the result is 'Nothing'.

-}
module TruthTable.WellFormedFormula
  ( Sign(..)
  , parsePL
  , isName
  , isBinOp
  ) where

data Sign = Name Char | And | Or | Impl | Not | OpenPar | ClosePar
  deriving (Show, Eq)


-- | Predicate to check that a Token is the result of the Name constructor.
isName :: Sign -> Bool
isName (Name _) = True
isName _ = False

-- | Predicate to check whether a Token corresponds to a binary logical operator
isBinOp :: Sign -> Bool
isBinOp t = t == And || t == Or || t == Impl

-- | Predicate for well parenthesized
wellParPred :: [Sign] -> Bool
wellParPred signs = wellParPred' 0 signs
  where wellParPred' :: Int -> [Sign] -> Bool
        wellParPred' 0 [] = True
        wellParPred' n [] = False
        wellParPred' n (x:xs)
          | n < 0 = False
          | x == OpenPar = wellParPred' (n + 1) xs
          | x == ClosePar = wellParPred' (n - 1) xs
          | otherwise = wellParPred' n xs

-- | Predicate for well formed propositions (except for parenthesis).
wellFormedPred :: [Sign] -> Bool
wellFormedPred [] = True
wellFormedPred expr = searchName expr


searchName :: [Sign] -> Bool
searchName [] = False
searchName (x:xs)
  | (isName x) = searchBinOp xs
  | x == Not = searchName xs
  | x == OpenPar = searchName xs
  | x == ClosePar = searchName xs
  | (isBinOp x) = False

searchBinOp :: [Sign] -> Bool
searchBinOp [] = True
searchBinOp (x:xs)
  | (isBinOp x) = searchName xs
  | (isName x) = False
  | x == Not = False
  | x == OpenPar = False
  | x == ClosePar = searchBinOp xs


-- | Parse a Propositional Logic Expression
parsePL :: [Char] -> Maybe [Sign]
parsePL exp = (wff (split exp)) 

-- | Turn a String into the list of its words
split :: [Char] -> [[Char]]
split expr = split' [] [] expr
  where split' :: [[Char]] -> [Char] -> [Char] -> [[Char]]
        split' accW [] [] = accW
        split' accW accR [] = accW ++ [accR]
        split' accW accR (x:xs)
          | x == ' ' && accR == [] = split' accW accR xs
          | x == ' ' = split' (accW ++ [accR]) [] xs
          | elem x "()" && accR /= [] = split' (accW ++ [accR, [x]]) [] xs
          | elem x "()" && accR == [] = split' (accW ++ [[x]]) [] xs
          | otherwise = split' accW (accR ++ [x]) xs

-- | Turn a list of 'String's into a 'List' of 'Sign's inside a 'Maybe' Functor.
wff :: [[Char]] -> Maybe [Sign]
wff strings = wff' [] strings
  where wff' :: [Sign] -> [[Char]] -> Maybe [Sign]
        wff' acc [] = if wellParPred acc && wellFormedPred acc
                               then Just acc
                               else Nothing
        wff' acc (x:xs)
          | x == "And" || x == "and" = wff' (acc ++ [And]) xs
          | x == "Or" || x == "or" = wff' (acc ++ [Or]) xs
          | x == "Impl" || x == "impl" = wff' (acc ++ [Impl]) xs
          | x == "Not" || x == "not" = wff' (acc ++ [Not]) xs
          | x == "(" = wff' (acc ++ [OpenPar]) xs
          | x == ")" = wff' (acc ++ [ClosePar]) xs
          | (length x) == 1 = wff' (acc ++ [(Name (head x))]) xs
          | otherwise = Nothing

-- | Select the set of the Names present in a list of 'Sign's
setOfNames :: [Sign] -> [Sign]
setOfNames input = setOfNames' [] input
  where setOfNames' :: [Sign] -> [Sign] -> [Sign]
        setOfNames' acc [] = acc
        setOfNames' acc (x:xs)
          | isName x == True && not (elem x acc) = setOfNames' (acc ++ [x]) xs
          | otherwise = setOfNames' acc xs

