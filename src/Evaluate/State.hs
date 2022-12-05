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
          , backtracking'stack :: [([Goal], Int, Map.Map String Value)]
            -- a stack of things to try when the current
            -- goal gails or succeeds

          , goal'stack :: [Goal]  -- goals to satisfy
          , position :: Int -- position in the base
          -- , environment :: Env  -- the unification structure
          , query'vars :: Map.Map String Value  -- the variables from the query

          , counter :: Int } -- for renaming variables
  deriving (Eq, Show)


-- TODO: A new state representation that encodes:
-- Processing a current goal'stack
-- Succeeded - does not contain the goal'stack (I think)
-- Failed - does not contain the goal'stack (might be interesting to think about how to represent what failed and why)
-- 
-- The main idea is that there is no Redoing and Done
-- and also the goal'stack is NonEmpty
-- this eliminates the need foor those two equations in `step`
-- because this book keeping will be done in a different function
-- a function that takes a state like Succeeded, one that does not contain a goal'stack
-- and either populates the goal'stack for Processing/Searching or decides that it is Done.
-- This seems like more sensible approach.


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
