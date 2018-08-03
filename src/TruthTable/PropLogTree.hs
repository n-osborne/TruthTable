{-|
Module      : TruthTable.PropLogTree
Description : This module build the tree corresponding to a given well formed formula
              of propositional logic.
Copyright   : (c) Nicolas Osborne, 2018
Licence     : GPL-3
Maintainer  : nicolas.osborne@etu.univ-lille1.fr

This module provides a modelisation of the well formed formulae of propositional
logic as a tree (named here 'PLTree' obviously for Propositional Logic Tree).

To build the 'PLTree' from a well formed formula provided by the 'TruthTable.WellFormedFormula'
module, we provide the 'buildTree' function which which initiate a Finite State
Machine. This FST is composed of three states. Each of these three states corresponds
to one of the three functions named 'state1', 'state2' and 'state3'. These functions
take three arguments:

* the 'PLTree' which has been build so far

* a list (used as a stack) of the 'PLTree' constructors waiting for a last 'PLTree' to be completed

* the part of the well formed formula which has still to be read (as a list of 'Sign's).


Each of the three states have a particular rôle and is waiting for some particular input:


== 'state1':

===== User Conditions:

* Waiting for 'Name', 'OpenPar' or 'Not'.
* First argument must be 'PLEmpty'.
* Second argument may be the empty list (in case of the empty formula).

===== When is it called?

'state1' is called at the initialisation and after the 'OpenPar' occurrences.

===== Action:

Begin a new (sub)expression.

+-----------+---------------------------------------------------------+
| input     | action                                                  |
+===========+=========================================================+
| 'Name'    | build the corresponding 'PLLeave' and call 'state2'     |
+-----------+---------------------------------------------------------+
| 'OpenPar' | continue the iteration of the wff                       |
+-----------+---------------------------------------------------------+
| 'Not'     | put a 'PLNot' at the top of the stack and call 'state3' |
+-----------+---------------------------------------------------------+

== 'state2':

===== User Conditions

* Waiting for 'And', 'Or', 'Impl' or 'ClosePar'.
* First argument can not be 'PLEmpty'.
* Second argument may be the empty list.

===== When is it called?

'state2' is called after the occurrence of a 'Name' and as a loop at the end of the iteration of
the Well Formed Formula in order to complete the 'PLTree'.

===== Action:

Either build a new 'PLTree' constructor at the top of the stack or output the result.

+------------+----------------+-------------------------------------------------------------------------+
| input      | stack          | action                                                                  |
+============+================+=========================================================================+
| binOp      | can't be empty | build 'PLTree' constructor with binOp as                                |
|            |                | ('PLTree' -> 'PLTree' -> 'PLTree') and 'PLTree'                         |
|            |                |  in first argument, then call 'state3'                                  |
+------------+----------------+-------------------------------------------------------------------------+
| 'ClosePar' | can't be empty | complete the 'PLTree' constructor at the top of the stack               |
|            |                | and the 'PLTree' in the first argument and continue iteration           |
+------------+----------------+-------------------------------------------------------------------------+
| empty list | not empty      | complete the 'PLTree' constructor at the top of the stack               |
|            |                | and the 'PLTree' in the first argument and continue with the empty list |
+------------+----------------+-------------------------------------------------------------------------+
| empty list | empty          | output 'PLTree' in first argument                                       |
+------------+----------------+-------------------------------------------------------------------------+

== 'state3':

===== User Conditions:

* Waiting for a 'Name', 'Not' or 'OpenPar'.
* First argument must be 'PLEmpty'.
* Second argument can not be the empty list.

===== When is it called?

'state3' is called after the occurence either of a 'Not' or one of the binary operator ('And', 'Or' or 'Impl').

===== Action:

Complete a 'PLTree' constructor (of type ('PLTree' -> 'PLTree')).

+-----------+-----------------------------------------------------------------------------+
| input     | action                                                                      |
+===========+=============================================================================+
| 'Name'    | complete the 'PLTree' constructor at the top of the stack and call 'state3' |
+-----------+-----------------------------------------------------------------------------+
| 'Not'     | put a 'PLNot' at the top of the stack and call 'sate1'                      |
+-----------+-----------------------------------------------------------------------------+
| 'OpenPar' | call 'sate1'                                                                |
+-----------+-----------------------------------------------------------------------------+

-}
module TruthTable.PropLogTree
  ( PLTree(..)
  , buildTree
  ) where

import TruthTable.WellFormedFormula

-- | Modelisation of a propositional logic well formed formula as a tree.
data PLTree = PLEmpty
            | PLLeave Sign
            | PLAnd PLTree PLTree
            | PLOr PLTree PLTree
            | PLImpl PLTree PLTree
            | PLNot PLTree
            deriving (Show, Eq)


-- | Initiate the building of the 'PLTree'.
buildTree :: [Sign] -> PLTree
buildTree wff = state1 PLEmpty [] wff

-- | First state of the FST, search for a 'Name', 'OpenPar' or 'Not'.
state1 :: PLTree -> [(PLTree -> PLTree)] -> [Sign] -> PLTree
state1 acc1 acc2 [] = acc1
state1 acc1 acc2 (x:xs)
  | isName x = state2 (PLLeave x) acc2 xs
  | x == OpenPar = state1 PLEmpty acc2 xs
  | x == Not = state3 PLEmpty (PLNot:acc2) xs

-- | Second state of the FST, search for a 'And', 'Or', 'Impl' or 'ClosePar'.
state2 :: PLTree -> [(PLTree -> PLTree)] -> [Sign] -> PLTree
state2 acc1 [] [] = acc1
state2 acc1 (x:xs) [] = state2 (x acc1) xs []
state2 acc1 acc2 (x:xs)
  | x == ClosePar = state2 ((head acc2) acc1) (drop 1 acc2) xs
  | isBinOp x = state3 PLEmpty (((fun x) acc1):acc2) xs
  where fun :: Sign -> (PLTree -> PLTree -> PLTree)
        fun And = PLAnd
        fun Or = PLOr
        fun Impl = PLImpl

-- | Third state of the FST, search for a 'Name', 'OpenPar' or 'Not'.
state3 :: PLTree -> [(PLTree -> PLTree)] -> [Sign] -> PLTree
state3 acc1 acc2 (x:xs)
  | isName x = state2 ((head acc2) (PLLeave x)) (drop 1 acc2) xs
  | x == Not = state3 PLEmpty (PLNot:acc2) xs
  | x == OpenPar = state1 PLEmpty acc2 xs
