module Visualize where

-- import Prelude hiding ( Functor, lookup )
-- import Data.List ( foldl', concatMap, intercalate )
-- import Data.Set qualified as Set
-- import Data.Map.Strict qualified as Map


-- import Term ( Value(..), Functor(..), Predicate(..), Goal(..) )

-- import Evaluate.State ( State(..), Action(..), Env, lookup, Var'State(..) )


-- free'vars'in'query :: [Goal] -> Set.Set String
-- free'vars'in'query goals = foldl' (\ set g -> set `Set.union` free'vars'in'goal g) Set.empty goals


-- free'vars'in'goal :: Goal -> Set.Set String
-- free'vars'in'goal (Call fun) = free'vars'in'functor fun
-- free'vars'in'goal (Unify val'l val'r) = Set.union (free'vars'in'val val'l) (free'vars'in'val val'r)


-- free'vars'in'functor :: Functor -> Set.Set String
-- free'vars'in'functor Fun{ args } = foldl' (\ set g -> set `Set.union` free'vars'in'val g) Set.empty args


-- free'vars'in'val :: Value -> Set.Set String
-- free'vars'in'val (Var name) = Set.singleton name
-- free'vars'in'val (Atom name) = Set.empty
-- free'vars'in'val (Struct fun) = free'vars'in'functor fun
-- free'vars'in'val Wildcard = Set.empty


-- show'assignments :: State -> String
-- show'assignments State{ vars, environment = env@(first, second) }
--   = result
--   where
--     show'assignment :: String -> [String]
--     show'assignment var'name
--       = case lookup var'name env of
--           Nothing -> []

--           Just (Fused vars') ->
--             let intersect = Set.intersection vars vars'
--             in  if Set.null intersect
--                 then []
--                 else [ var'name ++ " = " ++ intercalate " = " (Set.toList intersect) ]

--           Just (Assigned val) -> [ var'name ++ " = " ++ present'val val vars env ]

--           Just (Fused'Assigned _ val) -> [ var'name ++ " = " ++ present'val val vars env ]

--     assignments = concatMap show'assignment (Set.toList vars)
--     result = intercalate "\n" assignments
-- -- For each of `vars` I look it up in the environment.
-- -- If it is not even there, than this thing can be whatever => I do not print it.
-- -- If it is there and is Fused with something => I do not print it.
-- -- If it is there and is Assigned => I need to serialize/present that something.
-- -- If it is there is Fused'Assigned => I need to serialize/present that something.

-- -- When presenting a value I need to carry around a list/set of original "query variables"
-- -- whenever I find a variable (within the value) I need to search through my env.
-- -- If it is fused with some query variable (or more) I print it as that (one of) query variable.

-- -- I don't care about recursive values at this point.


-- present'val :: Value -> Set.Set String -> Env -> String
-- present'val (Var name) q'vars env
--   = case lookup name env of
--       Nothing ->
--         name
--       Just (Fused vars) ->
--         let intersect = Set.intersection q'vars vars
--         in  if Set.null intersect
--             then name
--             else head (Set.toList intersect) -- just pick the first one, whatever

--       Just (Assigned val) ->
--         present'val val q'vars env

--       Just (Fused'Assigned _ val) ->
--         present'val val q'vars env

-- present'val (Atom name) q'vars env = name

-- present'val (Struct fun) q'vars env = present'functor fun q'vars env

-- present'val Wildcard _ _ = "_"


-- present'functor :: Functor -> Set.Set String -> Env -> String
-- present'functor Fun{ name, args } q'vars env
--   = let
--     in  name ++ "(" ++ intercalate ", " (map (\ v -> present'val v q'vars env) args) ++ ")"
