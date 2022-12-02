module AST.Goal where

import AST.Term ( Functor(..), Term(..), Value(..) )
import AST.Predicate ( Predicate(..) )


-- TODO: Consider the following representation.
-- data Goal = Call Functor
--           | Unify Value Value
--   deriving (Eq, Show)
-- You'd then delete Term altogether.


data Goal = Prove Term
          | Unify Value Value
  deriving (Eq, Show)


-- we are proving propositions like
-- E = A
-- L = 23
-- foo(A, 23).
-- conjunctions of those above
--

-- we are unifying things like
-- variables
-- atoms
-- constants
-- functors
--