module Term where

import Prelude hiding ( Functor )

import Data.List ( intercalate )

data Goal = Call Functor
          | Unify Value Value
  deriving (Eq)


instance Show Goal where
  show (Call fun) = show fun
  show (Unify val'l val'r) = show val'l ++ " = " ++ show val'r


data Predicate  = Fact Functor
                | Functor :- [Goal]
  deriving (Eq)

instance Show Predicate where
  show (Fact fun) = show fun ++ "."
  show (head :- body) = show head ++ " :- " ++ intercalate " , " (map show body) ++ "."


-- arguments can only be things that are patterns
-- variable, constat, atom, struct
data Functor = Fun{ name :: String, args :: [Value] }
  deriving (Eq)

instance Show Functor where
  show Fun{ name, args } = name ++ "(" ++ intercalate ", " (map show args) ++ ")"


data Value  = Var String
            | Atom String
            | Struct Functor
            | Wildcard
  deriving (Eq)


instance Show Value where
  show (Var name) = name
  show (Atom name) = name
  show (Struct fun) = show fun
  show Wildcard = "_"
