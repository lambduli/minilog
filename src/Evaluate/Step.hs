module Evaluate.Step where

import Prelude hiding ( Functor )

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List ( foldl', mapAccumL )


import Evaluate.State ( State(..), Env, Var'State(..) )

import AST.Goal ( Goal(Unify, Prove) )
import AST.Predicate ( Predicate(..) )
import AST.Term ( Term(Call, Conjunction), Functor(..), Value(..) )


step :: State -> State
-- in these two equations we handle the situation
-- when in the previous step we have successfully proved the whole goal
-- that leaves us with an empty goal'stack
-- so we attempt to backtrack
-- in case there are no records on the backtracking'stack we just return the same state
step state@State{ backtracking'stack = []
                , goal'stack = [] }
  = state

step state@State{ base
                , backtracking'stack = record : backtracking'stack
                , goal'stack = []
                , position
                , environment
                , counter }
  = state{ goal'stack = new'goal'stack, position = pos, environment = env, backtracking'stack }
  where (new'goal'stack, pos, env) = record

-- in this equation we handle the situation
-- when the top'goal is to Prove a conjunction of two terms
-- this is done by putting two new goals at the top of the goal'stack
-- I also need to reset the position
-- which I can not do
-- TODO: So I really need to think this one through!
-- do I keep the position in the Goal?
-- does it ever happen that I would prove the top'goal
-- pop it
-- and need to continue proving something at the bottom that I have had
-- already worked on?
-- I am not sure.
-- I think this does not happen for backtracking.
-- Because backtracking is done using entirely different goal'stack.
-- So does that mean, that when I successfully prove something, I pop it from the stack,
-- and below it, there can only be some thing that haven't been tried to be proven yet?
-- It is a fresh goal.
-- So when I succeed with one goal and pop, I would also reset the counter?
-- It feels like maybe yes, I will need to think about it more.
step state@State{ base
                , backtracking'stack
                , goal'stack = Prove (Conjunction term'a term'b) : goal'stack
                , position
                , environment
                , counter }
  = state{ goal'stack = new'goal'stack, position = 0 }
  -- The order is important!
  where new'goal'stack = Prove term'a : Prove term'b : goal'stack

{-  PROVE CALL  -}
step state@State{ base
                , backtracking'stack
                , goal'stack = gs@(Prove (Call (f@Fun{ name, args })) : goal'stack)
                , position
                , environment
                , counter }
  = case look'for f (drop position base) position of
      Nothing -> fail'and'backtrack state

      Just (Fact (Fun{ args = patterns }), the'position) ->
        let (counter', patterns') = rename'all patterns counter
            goals = map (uncurry Unify) (zip args patterns')
            new'goal'stack = goals ++ goal'stack

            bt'stack = gs
            bt'pos = the'position + 1
            bt'env = environment
            backtracking'record = (bt'stack, bt'pos, bt'env)

            new'state =  state{ backtracking'stack = backtracking'record : backtracking'stack
                              , goal'stack = new'goal'stack
                              , position = 0  -- the current goal will never ever be tried again (in this goal'stack anyway)
                              , counter = counter' }

        in  new'state

      Just (Fun{ args = patterns } :- body, the'position) -> 
        let (counter', patterns', body') = rename'both patterns body counter
            head'goals = map (uncurry Unify) (zip args patterns')
            body'goal = Prove body'
            new'goal'stack = head'goals ++ [body'goal] ++ goal'stack

            bt'stack = gs
            bt'pos = the'position + 1
            bt'env = environment
            backtracking'record = (bt'stack, bt'pos, bt'env)

            new'state =  state{ backtracking'stack = backtracking'record : backtracking'stack
                              , goal'stack = new'goal'stack
                              , position = 0  -- the current goal will never ever be tried again
                              , counter = counter' }

        in  new'state

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

-- in this equation we handle the situation
-- when the goal is to Unify two Values
-- the unification function will take care of that
-- the unification can possibly change the environment
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
        state { goal'stack
              , environment = new'env }


-- the following function fails the current goal
-- it needs to replace the current goal'stack with a top of the backtracking one
-- that means re-setting position and environment
-- counter stays the same (because it can)
-- 
fail'and'backtrack :: State -> State
fail'and'backtrack state@State{ backtracking'stack = [] }
  = state{ goal'stack = [] }

fail'and'backtrack state@State{ backtracking'stack = backtrack'record : backtracking'stack }
  = step state{ backtracking'stack
              , goal'stack = new'goal'stack
              , position = pos
              , environment = env }
  where (new'goal'stack, pos, env) = backtrack'record


rename'all :: [Value] -> Int -> (Int, [Value])
rename'all patterns counter = (counter', patterns')
  where
    ((counter', mapping), patterns') = mapAccumL rename (counter, Map.empty) patterns


rename :: (Int, Map.Map String String) -> Value -> ((Int, Map.Map String String), Value)
rename (cntr, mapping) (Var name)
  = if Map.member name mapping
    then ((cntr, mapping), Var (mapping Map.! name))
    else  let new'name = "_" ++ show cntr
              new'cntr = cntr + 1
              new'mapping = Map.insert name new'name mapping
          in  ((new'cntr, new'mapping), Var new'name)

rename state (Struct (Fun{ name, args }))
  = let (state', args') = mapAccumL rename state args
    in  (state', Struct (Fun{ name = name, args = args' }))

rename acc val
  = (acc, val)


rename'both :: [Value] -> Term -> Int -> (Int, [Value], Term)
rename'both patterns body counter = (counter', patterns', body')
  where
    (state, patterns') = mapAccumL rename (counter, Map.empty) patterns

    (state', body') = rename'term state body

    (counter', _) = state'


rename'term :: (Int, Map.Map String String) -> Term -> ((Int, Map.Map String String), Term)
rename'term state'0 (Conjunction term'l term'r) = (state'2, Conjunction term'l'1 term'r'1)
  where
    (state'1, term'l'1) = rename'term state'0 term'l
    (state'2, term'r'1) = rename'term state'1 term'r

rename'term state'0 (Call fun) = (state'1, Call fun')
  where
    (state'1, Struct fun') = rename state'0 (Struct fun)



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
