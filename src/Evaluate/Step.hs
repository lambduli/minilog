module Evaluate.Step where

import Prelude hiding ( Functor )

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List ( foldl', mapAccumL )


import Evaluate.State ( State(..), Env, Var'State(..), Action(..) )

import Term ( Predicate(..), Functor(..), Value(..), Goal(..) )


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
  where (new'goal'stack, pos, env) = record
        state' = state{ goal'stack = new'goal'stack
                      , position = pos
                      , environment = env
                      , backtracking'stack }

{-  PROVE CALL  -}
step state@State{ base
                , backtracking'stack
                , goal'stack = gs@(Call (f@Fun{ name, args }) : goal'stack)
                , position
                , environment
                , counter }
  = case look'for f (drop position base) position of
      Nothing -> fail'and'backtrack state

      Just (Fact (Fun{ args = patterns }), the'position) ->
        let (counter', patterns') = rename'all patterns counter
            goals = map (uncurry Unify) (zip args patterns')
            new'goal'stack = goals ++ goal'stack

            backtracking'stack' = cause'backtracking f base the'position gs environment backtracking'stack

            new'state =  state{ backtracking'stack = backtracking'stack'
                              , goal'stack = new'goal'stack
                              , position = 0  -- the current goal will never ever be tried again (in this goal'stack anyway)
                              , counter = counter' }

        in  Searching new'state

      Just (Fun{ args = patterns } :- body, the'position) -> 
        let (counter', patterns', body') = rename'both patterns body counter
            head'goals = map (uncurry Unify) (zip args patterns')
            new'goal'stack = head'goals ++ body' ++ goal'stack

            backtracking'stack' = cause'backtracking f base the'position gs environment backtracking'stack

            new'state =  state{ backtracking'stack = backtracking'stack'
                              , goal'stack = new'goal'stack
                              , position = 0  -- the current goal will never ever be tried again
                              , counter = counter' }

        in  Searching new'state

  where look'for :: Functor -> [Predicate] -> Int -> Maybe (Predicate, Int)
        look'for _ [] _ = Nothing
        -- a fact with the same name and arity
        look'for f@Fun{ name, args } (fact@(Fact (Fun{ name = name', args = args' })) : base) pos
          | name == name' && length args == length args' = Just (fact, pos)
          | otherwise = look'for f base (pos + 1)
        -- | Functor :- Term
        -- a rule with the same name and arity
        look'for f@Fun{ name, args } (rule@(Fun{ name = name', args = args' } :- body) : base) pos
          | name == name' && length args == length args' = Just (rule, pos)
          | otherwise = look'for f base (pos + 1)


        cause'backtracking :: Functor -> [Predicate] -> Int -> [Goal] -> Env -> [([Goal], Int, Env)] -> [([Goal], Int, Env)]
        cause'backtracking f base position goal'stack env backtracking'stack
          = case look'for f (drop position base) position of
              Nothing -> backtracking'stack
              Just (_, future'position) ->
                let backtracking'record = (goal'stack, future'position, env)
                in  backtracking'record : backtracking'stack

{-  PROVE UNIFICATION -}
step state@State{ base
                , backtracking'stack
                , goal'stack = Unify value'l value'r : goal'stack
                , position
                , environment
                , counter }
  = case unify value'l value'r environment of
      Nothing ->
        -- could not unify
        -- this means that this goal, fails
        fail'and'backtrack state
      Just new'env ->
        -- they can be unified and the new'environment reflects that
        -- just return a new state with stack and env changed
        succeed state { goal'stack
                      , environment = new'env }


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
              , environment = env }
  where (new'goal'stack, pos, env) = backtrack'record


rename'all :: [Value] -> Int -> (Int, [Value])
rename'all patterns counter = (counter', patterns')
  where
    ((counter', mapping), patterns') = mapAccumL rename'val (counter, Map.empty) patterns


rename'val :: (Int, Map.Map String String) -> Value -> ((Int, Map.Map String String), Value)
rename'val (cntr, mapping) (Var name)
  = if Map.member name mapping
    then ((cntr, mapping), Var (mapping Map.! name))
    else  let new'name = "_" ++ show cntr
              new'cntr = cntr + 1
              new'mapping = Map.insert name new'name mapping
          in  ((new'cntr, new'mapping), Var new'name)

rename'val state (Struct (Fun{ name, args }))
  = let (state', args') = mapAccumL rename'val state args
    in  (state', Struct (Fun{ name = name, args = args' }))

rename'val acc val
  = (acc, val)


rename'both :: [Value] -> [Goal] -> Int -> (Int, [Value], [Goal])
rename'both patterns goals counter = (counter', patterns', goals')
  where
    (state, patterns') = mapAccumL rename'val (counter, Map.empty) patterns

    (state', goals') = mapAccumL rename'goal state goals

    (counter', _) = state'


rename'goal :: (Int, Map.Map String String) -> Goal -> ((Int, Map.Map String String), Goal)
rename'goal state (Call (Fun{ name, args }))
  = let (state', args') = mapAccumL rename'val state args
    in  (state', Call (Fun{ name, args = args' }))
rename'goal state (Unify val'l val'r)
  = let (state', [val'l', val'r']) = mapAccumL rename'val state [val'l, val'r]
    in  (state', Unify val'l' val'r')


unify :: Value -> Value -> Env -> Maybe Env
unify Wildcard _ env
  = Just env

unify (Atom a) (Atom b) env
  | a == b = Just env
  | otherwise = Nothing

unify (Number a) (Number b) env
  | a == b = Just env
  | otherwise = Nothing

unify (Struct (Fun{ name = name'l, args = args'l }))
      (Struct (Fun{ name = name'r, args = args'r }))
      env
        | name'l == name'r && length args'l == length args'r = unify' args'l args'r
        | otherwise = Nothing
        where unify' :: [Value] -> [Value] -> Maybe Env
              unify' lefts rights = foldl' unify'' (Just env) (zip lefts rights)

              unify'' :: Maybe Env -> (Value, Value) -> Maybe Env
              unify'' Nothing _ = Nothing
              unify'' (Just env) (left, right) = unify left right env

unify (Var v'l) (Var v'r) env@(first, second)
  | v'l == v'r = Just env
  | otherwise = case (Map.member v'l first, Map.member v'r first) of
                  (False, False) -> -- FRESH ~ FRESH
                    let new'addr    = Map.size second
                        new'first   = Map.insert v'l new'addr $! Map.insert v'r new'addr first
                        new'second  = Map.insert new'addr (Fused $! Set.fromList [v'l, v'r]) second
                    in  Just (new'first, new'second)

                  (False, True) ->  -- FRESH ~ ?
                    let addr = first Map.! v'r
                    in  case second Map.! addr of
                          Fused vars -> -- more fusing
                            let new'first = Map.insert v'l addr first -- register the fresh variable
                                new'second = Map.insert addr (Fused $! Set.insert v'l vars) second -- just adds it into the set of vars
                            in  Just (new'first, new'second)

                          -- TODO: occurs check!
                          Assigned val -> -- from assigned to fused'assigned
                            let new'first = Map.insert v'l addr first -- register the fresh variable
                                new'second = Map.insert addr (Fused'Assigned (Set.singleton v'l) val) second -- promote to fused'assigned
                            in  Just (new'first, new'second)

                          -- TODO: occurs check!
                          Fused'Assigned vars val ->  -- more fusing
                            let new'first = Map.insert v'l addr first -- register the fresh variable
                                new'second = Map.insert addr (Fused'Assigned (Set.insert v'l vars) val) second -- add into the set
                            in  Just (new'first, new'second)

                  (True, False) ->  -- ? ~ FRESH
                    unify (Var v'r) (Var v'l) env -- a little trick

                  (True, True) ->   -- ? ~ ?
                    let addr'l = first Map.! v'l
                        addr'r = first Map.! v'r
                    in  case (second Map.! addr'l, second Map.! addr'r) of
                          (Fused vars'l, Fused vars'r) ->
                            let new'first = Map.insert v'l addr'r first
                                new'second = Map.insert addr'r (Fused $! Set.union vars'l vars'r) second
                            in  Just (new'first, new'second)

                          -- TODO: occurs check! check even those fused
                          (Fused vars'l, Assigned val'r) ->
                            let new'first = Map.insert v'l addr'r first
                                new'second = Map.insert addr'r (Fused'Assigned vars'l val'r) second
                            in  Just (new'first, new'second)

                          -- TODO: occurs check! check even those fused
                          (Fused vars'l, Fused'Assigned vars'r val'r) ->
                            let new'first = Map.insert v'l addr'r first
                                new'second = Map.insert addr'r (Fused'Assigned (Set.union vars'l vars'r) val'r) second
                            in  Just (new'first, new'second)

                          -- TODO: occurs check! check both ways
                          (Assigned val'l, Assigned val'r) ->
                            unify val'l val'r env
                            -- NOTE: The thing is, even if it succeeds, I can't really reflect that in the Env.
                            -- Because I do not subsitute variables within structs when some variable is assigned a value,
                            -- those two things do not look alike.
                            -- one might look like this: `foo(A, B, x, 1)` the other like: `foo(thing, X, X, 1)`
                            -- Instead of just picking arbitrary one of them I do not pick at all.
                            -- But I do believe that any choice would do.

                          -- TODO: occurs check! check both ways and fused too
                          (Assigned val'l, Fused'Assigned vars'r val'r) ->
                            unify val'l val'r env
                            -- the same reasoning as above

                          -- TODO: occurs check! check both ways and fused too
                          (Fused'Assigned vars'l val'l, Fused'Assigned vars'r val'r) ->
                            unify val'l val'r env
                            -- the same reasoning as above

                          (_, _) -> unify (Var v'r) (Var v'l) env -- a little trick

unify (Var v'l) value env@(first, second)
  = case first Map.!? v'l of
    Just addr ->  -- fused, assigned, or fused'assigned
      case second Map.! addr of
        Fused vars -> -- promote to fused'assigned
          let new'second = Map.insert addr (Fused'Assigned vars value) second
          in  Just (first, new'second)

        -- TODO: occurs check!
        Assigned value' ->  -- unify those two values, no new binding
          unify value value' env

        -- TODO: occurs check!
        Fused'Assigned _ value' -> -- unify those two values, no new binding
          unify value value' env

    Nothing ->  -- a fresh variable
      let new'addr = Map.size second
          new'first = Map.insert v'l new'addr first
          new'second = Map.insert new'addr (Assigned value) second
      in  Just (new'first, new'second)

unify value (Var v'r) env
  = unify (Var v'r) value env -- a little trick
