module Term where

import Data.List ( intercalate )


data Goal = Call !Struct
          | Unify !Term !Term
  deriving (Eq)


data Predicate  = Fact !Struct
                | !Struct :- ![Goal]
  deriving (Eq)


data Struct = Struct{ name :: !String, args :: ![Term] }
  deriving (Eq)


data Term = Var !String
          | Atom !String
          | Compound !Struct
          | Wildcard
  deriving (Eq)


instance Show Goal where
  show (Call struct) = show struct
  show (Unify val'l val'r) = show val'l ++ " = " ++ show val'r


instance Show Predicate where
  show (Fact struct) = show struct ++ "."
  show (head :- body) = show head ++ " :- " ++ intercalate " , " (map show body) ++ "."


instance Show Struct where
  show Struct{ name, args } = name ++ "(" ++ intercalate ", " (map show args) ++ ")"


instance Show Term where
  show (Var name) = name
  show (Atom name) = name
  show (Compound struct) = show struct
  show Wildcard = "_"
