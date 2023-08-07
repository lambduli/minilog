module Evaluate.Step where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List ( mapAccumL )


import Evaluate.State ( State(..), Action(..) )

import Term ( Predicate(..), Struct(..), Term(..), Goal(..) )


step :: State -> Action State
-- In these two equations we handle the situation
-- when in the previous step we have successfully proved the whole goal
-- that leaves us with an empty goal'stack.
-- These two equations handle the situation when no backtracking can happen (empty backtracking'stack)
-- or if some backtracking can happen (Redoing).
-- It's not ideal as the idea that the Action transitions from Succeede to either Done or Redoing is only in our heads.
-- It would be much better if it could be encoded in the design so that the type system and pattern matching
-- exhaustivity checker would have our backs, but it is what it is.
step state@State{ backtracking'stack = []
                , goal'stack = [] }
  = Done

step state@State{ backtracking'stack = record : backtracking'stack
                , goal'stack = [] }
  = Redoing state'
  where (new'goal'stack, pos, q'vars) = record
        state' = state{ goal'stack = new'goal'stack
                      , position = pos
                      , query'vars = q'vars
                      , backtracking'stack }

{-  PROVE CALL  -}
step state@State{ base
                , backtracking'stack
                , goal'stack = gs@(Call (f@Struct{ name, args }) : goal'stack)
                , position
                , query'vars
                , counter }
  = case look'for f (drop position base) position of
      Nothing -> fail'and'backtrack state

      Just (Fact (Struct{ args = patterns }), the'position) ->
        let (counter', patterns') = rename'all patterns counter
            goals = map (uncurry Unify) (zip args patterns')
            new'goal'stack = goals ++ goal'stack

            backtracking'stack' = cause'backtracking f base (the'position + 1) gs query'vars backtracking'stack

            new'state =  state{ backtracking'stack = backtracking'stack'
                              , goal'stack = new'goal'stack
                              , position = 0  -- the current goal will never ever be tried again (in this goal'stack anyway)
                              , counter = counter' }

        in  Searching new'state

      Just (Struct{ args = patterns } :- body, the'position) -> 
        let (counter', patterns', body') = rename'both patterns body counter
            head'goals = map (uncurry Unify) (zip args patterns')
            new'goal'stack = head'goals ++ body' ++ goal'stack

            backtracking'stack' = cause'backtracking f base (the'position + 1) gs query'vars backtracking'stack

            new'state =  state{ backtracking'stack = backtracking'stack'
                              , goal'stack = new'goal'stack
                              , position = 0  -- the current goal will never ever be tried again
                              , counter = counter' }

        in  Searching new'state

  where look'for :: Struct -> [Predicate] -> Int -> Maybe (Predicate, Int)
        look'for _ [] _ = Nothing
        -- a fact with the same name and arity
        look'for f@Struct{ name, args } (fact@(Fact (Struct{ name = name', args = args' })) : base) pos
          | name == name' && length args == length args' = Just (fact, pos)
          | otherwise = look'for f base (pos + 1)
        -- | Struct :- Term
        -- a rule with the same name and arity
        look'for f@Struct{ name, args } (rule@(Struct{ name = name', args = args' } :- body) : base) pos
          | name == name' && length args == length args' = Just (rule, pos)
          | otherwise = look'for f base (pos + 1)


        cause'backtracking :: Struct -> [Predicate] -> Int -> [Goal] -> Map.Map String Term -> [([Goal], Int, Map.Map String Term)] -> [([Goal], Int, Map.Map String Term)]
        cause'backtracking f base position goal'stack q'vars backtracking'stack
          = case look'for f (drop position base) position of
              Nothing -> backtracking'stack
              Just (_, future'position) ->
                let backtracking'record = (goal'stack, future'position, q'vars)
                in  backtracking'record : backtracking'stack

{-  PROVE UNIFICATION -}
step state@State{ base
                , backtracking'stack
                , goal'stack = Unify value'l value'r : goal'stack
                , position
                , query'vars
                , counter }
  = case unify (value'l, value'r) goal'stack query'vars of
      Nothing ->
        -- could not unify
        -- this means that this goal, fails
        fail'and'backtrack state
      Just (new'goal'stack, new'query'vars) ->
        -- they can be unified and the new'environment reflects that
        -- just return a new state with stack and env changed
        succeed state { goal'stack = new'goal'stack, query'vars = new'query'vars }


succeed :: State -> Action State
succeed state@State{ goal'stack = [] }
  = Succeeded state

succeed state
  = Searching state


-- The following function fails the current goal.
-- It needs to replace the current goal'stack with a top of the backtracking one.
-- That means re-setting the position and the environment.
-- The counter stays the same (because it only increments).
fail'and'backtrack :: State -> Action State
fail'and'backtrack state@State{ backtracking'stack = [] }
  = Failed

fail'and'backtrack state@State{ backtracking'stack = backtrack'record : backtracking'stack }
  = step state{ backtracking'stack
              , goal'stack = new'goal'stack
              , position = pos
              , query'vars = q'vars }
  where (new'goal'stack, pos, q'vars) = backtrack'record


rename'all :: [Term] -> Int -> (Int, [Term])
rename'all patterns counter = (counter', patterns')
  where
    ((counter', mapping), patterns') = mapAccumL rename'val (counter, Map.empty) patterns


rename'val :: (Int, Map.Map String String) -> Term -> ((Int, Map.Map String String), Term)
rename'val (cntr, mapping) (Var name)
  = if Map.member name mapping
    then ((cntr, mapping), Var (mapping Map.! name))
    else  let new'name = "_" ++ show cntr
              new'cntr = cntr + 1
              new'mapping = Map.insert name new'name mapping
          in  ((new'cntr, new'mapping), Var new'name)

rename'val state (Compound (Struct{ name, args }))
  = let (state', args') = mapAccumL rename'val state args
    in  (state', Compound (Struct{ name = name, args = args' }))

rename'val acc val
  = (acc, val)


rename'both :: [Term] -> [Goal] -> Int -> (Int, [Term], [Goal])
rename'both patterns goals counter = (counter', patterns', goals')
  where
    (state, patterns') = mapAccumL rename'val (counter, Map.empty) patterns

    (state', goals') = mapAccumL rename'goal state goals

    (counter', _) = state'


rename'goal :: (Int, Map.Map String String) -> Goal -> ((Int, Map.Map String String), Goal)
rename'goal state (Call (Struct{ name, args }))
  = let (state', args') = mapAccumL rename'val state args
    in  (state', Call (Struct{ name, args = args' }))
rename'goal state (Unify val'l val'r)
  = let (state', [val'l', val'r']) = mapAccumL rename'val state [val'l, val'r]
    in  (state', Unify val'l' val'r')


unify :: (Term, Term) -> [Goal] -> Map.Map String Term -> Maybe ([Goal], Map.Map String Term)
{-  DELETE  (basically) -}
unify (Wildcard, _) goals query'vars = Just (goals, query'vars)
unify (_, Wildcard) goals query'vars = Just (goals, query'vars)

{-  DELETE  -}
unify (Atom a, Atom b) goals query'vars
  | a == b = Just (goals, query'vars)
  | otherwise = Nothing

{-  DECOMPOSE + CONFLICT  -}
unify ( Compound Struct{ name = name'a, args = args'a }
      , Compound Struct{ name = name'b, args = args'b })
      goals query'vars
  | name'a /= name'b || length args'a /= length args'b = Nothing  -- CONFLICT
  | otherwise = Just (arg'goals ++ goals, query'vars)             -- DECOMPOSE
  where
    arg'goals :: [Goal]
    arg'goals = zipWith Unify args'a args'b

{-  ELIMINATE + OCCURS  -}
unify (Var a, value) goals query'vars
  | (Var a) == value = Just (goals, query'vars) -- DELETE (both are variables)
  | occurs a value = Nothing                    -- OCCURS CHECK (the one on the right is not a variable so I can do the check!)
  | otherwise = Just (substituted'goals, substituted'query'vars)
  where
    substituted'goals = map (subst'goal (a, value)) goals
    substituted'query'vars = Map.map (subst'val (a, value)) query'vars

    subst'goal :: (String, Term) -> Goal -> Goal
    subst'goal substitution (Call fun) = Call substituted'fun
      where substituted'fun = subst'functor substitution fun
    subst'goal substitution (Unify val'a val'b) = Unify substituted'val'a substituted'val'b
      where substituted'val'a = subst'val substitution val'a
            substituted'val'b = subst'val substitution val'b

    subst'val :: (String, Term) -> Term -> Term
    subst'val (from, to) (Var name)
      | name == from = to
      | otherwise = Var name
    subst'val _ (Atom name) = Atom name
    subst'val substitution (Compound fun) = Compound (subst'functor substitution fun)
    subst'val _ Wildcard = Wildcard

    subst'functor :: (String, Term) -> Struct -> Struct
    subst'functor substitution Struct{ name, args } = Struct{ name, args = substituted'args }
      where substituted'args = map (subst'val substitution) args

{-  SWAP  (because of the above equation, we assume the `value` not being a variable) -}
unify (value, Var b) goals query'vars = unify (Var b, value) goals query'vars

unify _ _ _ = Nothing   -- CONFLICT (for atoms and structs)


occurs :: String -> Term -> Bool
occurs var'name (Var name) = var'name == name
occurs var'name (Atom _) = False
occurs var'name (Compound Struct{ args }) = any (occurs var'name) args
occurs var'name Wildcard = False
