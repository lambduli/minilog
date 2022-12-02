module Evaluate.State where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Term ( Goal, Value, Predicate )


data State = State  { base :: [Predicate]
                    , backtracking'stack :: [([Goal], Int, Env)]

                    , goal'stack :: [Goal]
                    , position :: Int
                    , environment :: Env

                    , counter :: Int } -- for renaming variables
  deriving (Eq, Show)


type Env = (Map.Map String Int, Map.Map Int Var'State)


data Var'State  = Fused (Set.Set String)
                | Assigned Value
                | Fused'Assigned (Set.Set String) Value
  deriving (Eq, Show)
