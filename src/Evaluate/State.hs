module Evaluate.State where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Term ( Goal, Value, Predicate )


{-  The Action data structure is there
    to signalize what happened in the last step -}
data Action a = Succeeded a
              | Failed
              | Searching a
              | Redoing a
              | Done
  deriving (Eq, Show)

-- TODO: Keep the original goal around.
data State
  = State { base :: [Predicate] -- knowledge base
          , vars :: Set.Set String  -- variables fro the query
          , backtracking'stack :: [([Goal], Int, Env)]
            -- a stack of things to try when the current
            -- goal gails or succeeds

          , goal'stack :: [Goal]  -- goals to satisfy
          , position :: Int -- position in the base
          , environment :: Env  -- the unification structure

          , counter :: Int } -- for renaming variables
  deriving (Eq, Show)


type Env = (Map.Map String Int, Map.Map Int Var'State)

lookup :: String -> Env -> Maybe Var'State
lookup name (first, second)
  = if not (Map.member name first)
    then Nothing
    else second Map.!? (first Map.! name)


{-  When a variable is inserted into the envionment
    it means it is either:
      Fused with another one (or multiple)
      Assigned a concrete (but possibly incomplete) value/term
      both of the above.  -}
data Var'State  = Fused (Set.Set String)
                | Assigned Value -- TODO: add String for the var name
                | Fused'Assigned (Set.Set String) Value
  deriving (Eq, Show)
