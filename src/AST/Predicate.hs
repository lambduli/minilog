module AST.Predicate where

import Prelude hiding ( Functor )

import AST.Term ( Term, Functor )

-- TODO: Consider the following representation.
-- data Predicate  = Functor :- [Goal] -- facts have an empty body
--   deriving (Eq, Show)



data Predicate  = Fact Functor
                | Functor :- Term
  deriving (Eq)

instance Show Predicate where
  show (Fact fun) = show fun ++ "."
  show (head :- body) = show head ++ " :- " ++ show body ++ "."
