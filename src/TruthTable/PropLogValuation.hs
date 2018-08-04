{-|
Module      : TruthTable.PropLogValuations
Description : This module compute the truth table.
Copyright   : (c) Nicolas Osborne, 2018
Licence     : GPL-3
Maintainer   : nicolas.osborne@etu.univ-lille3.fr

This module provides a function that computes the truth table and a predicate
that check whether a given proposition of the propositional logic is a tautology,
provided that it is given both in the form of a 'List' of 'Sign' and its
'PLTree' form.

== 'valuation' function

Both the computation of the truth table and the tautology predicate rely on the
'valuation' function.

This 'valuation' function takes two arguments:

* a 'PLTree'
* and a 'List' of 'Tuple' containing an arrangement of the truth values for
all the 'Name's contained in the 'PLTree'.

Then if traverse the tree in post-order replacing each node by a 'Bool'.

In order to compute the truth table or the value of the tautology predicate with
this 'valuation' function, we need all the possible arrangements of the truth values
for the 'Name's contained in the 'PLTree'.

== 'setOfNames' function

Select the set of all the 'Name's of a 'List' of 'Sign's.

* all the 'Name's present in the input are present in the output
* each 'Name' occurs only once in the output

== 'truthValuesArr' function

Given a set of 'Name's (but in a 'List'), return all the possible arrangements of
truth values in a 'List' of 'List' of tuple '(Sign, Bool)'.
-}
module TruthTable.PropLogValuation
  ( setOfNames
  , truthValuesArr
  , valuation
  , isTautology
  , truthTable
  ) where

import TruthTable.WellFormedFormula
import TruthTable.PropLogTree

-- | Select the set of the Names present in a list of 'Sign's.
setOfNames :: [Sign] -> [Sign]
setOfNames input = setOfNames' [] input
  where setOfNames' :: [Sign] -> [Sign] -> [Sign]
        setOfNames' acc [] = acc
        setOfNames' acc (x:xs)
          | (isName x) && not (elem x acc) = setOfNames' (x:acc) xs
          | otherwise = setOfNames' acc xs

-- | Compute all the possible arrangements of truth value for a given set of 'Name's
truthValuesArr :: [Sign] -> [[(Sign, Bool)]] -> [[(Sign, Bool)]]
truthValuesArr [] acc = acc
truthValuesArr (x:xs) [] = truthValuesArr xs [[(x, True)], [(x, False)]]
truthValuesArr (x:xs) acc = truthValuesArr xs ((map (addT x) acc) ++ (map (addF x) acc))
  where addT name acc = (name, True):acc
        addF name acc = (name, False):acc

-- | Compute the valuation of a propositional logic formula according to an
-- | arrangement of truth value.
valuation :: PLTree -> [(Sign, Bool)] -> Bool
valuation PLEmpty arr = True
valuation (PLAnd left right) arr = (valuation left arr) && (valuation right arr)
valuation (PLOr left right) arr = (valuation left arr) || (valuation right arr)
valuation (PLNot tree) arr = not (valuation tree arr)
valuation (PLImpl left right) arr = (not (valuation right arr)) || (valuation left arr)
valuation (PLLeave name) arr = findBool name arr
  where findBool :: Sign -> [(Sign, Bool)] -> Bool
        findBool name (x:xs)
          | name == (fst x) = snd x
          | otherwise = findBool name xs

-- | Predicate to check whether a propositional logic formula is a tautology.
isTautology :: PLTree -> [[(Sign, Bool)]] -> Bool
isTautology tree [] = True
isTautology tree (x:xs)
  | valuation tree x = isTautology tree xs
  | otherwise = False

-- | Compute the truth table of a given propositional logic formula.
truthTable :: PLTree -> [[(Sign, Bool)]] -> [([(Sign, Bool)], Bool)]
truthTable tree [] = []
truthTable tree (x:xs) = (x, (valuation tree x)):(truthTable tree xs)
