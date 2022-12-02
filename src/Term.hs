module Term where

import Prelude hiding ( Functor )

import Data.List ( intercalate )

data Goal = Call Functor
          | Unify Value Value
  deriving (Eq, Show)


data Predicate  = Fact Functor
                | Functor :- [Goal]
  deriving (Eq)

instance Show Predicate where
  show (Fact fun) = show fun ++ "."
  show (head :- body) = show head ++ " :- " ++ show body ++ "."


-- arguments can only be things that are patterns
-- variable, constat, atom, struct
data Functor = Fun{ name :: String, args :: [Value] }
  deriving (Eq)

instance Show Functor where
  show Fun{ name, args } = name ++ "(" ++ intercalate ", " (map show args) ++ ")"

-- TODO: Consider deleting the Term
-- data Term = Conjunction Term Term
--           | Call Functor
--           -- | Unify'Op Value Value -- not necessary, strictly speaking
--   deriving (Eq)

-- instance Show Term where
--   show (Conjunction term'a term'b) = show term'a ++ " , " ++ show term'b
--   show (Call fun) = show fun


data Value  = Var String
            | Atom String
            | Number Int
            | Struct Functor
            | Wildcard
  deriving (Eq)


instance Show Value where
  show (Var name) = name
  show (Atom name) = name
  show (Number int) = show int
  show (Struct fun) = show fun
  show Wildcard = "_"
