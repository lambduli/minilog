module Main where

import Prelude hiding ( Functor )

import Data.List ( foldl', concatMap, intercalate )

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set


import System.IO ( hFlush, stdout )

import Term ( Value(..), Functor(..), Predicate(..), Goal(..) )

import Evaluate.Step ( step )
import Evaluate.State ( State(..), Action(..) )

import Parser ( parse'base, parse'query )


small'base :: String
small'base =  "id(I,I)." ++ "\n" ++
              "foo(X, thing)." ++ "\n"

fact'base :: [Predicate]
fact'base = parse'base $!
            "plus(z, N, N)." ++ "\n" ++
            "plus(s(N), M, s(R)) :- plus(N, M, R)." ++ "\n" ++
            "times(z, _, z)." ++ "\n" ++
            "times(s(N), M, A) :- times(N, M, R), plus(R, M, A)." ++ "\n" ++
            "fact(z, s(z))." ++ "\n" ++
            "fact(s(N), R) :- fact(N, PR), times(s(N), PR, R)." ++ "\n"


init'base :: [Predicate]
init'base = [ Fact Fun{ name = "id", args = [ Var "I", Var "I" ] }
            , Fact Fun{ name = "foo", args = [ Var "X", Atom "thing" ] }
            ]


goal :: Goal
-- goal = Prove (Call (Fun{ name = "foo", args = [ Atom "something", Var "Y" ] }))
goal = Call (Fun{ name = "id", args = [ Var "Y", Struct (Fun{ name = "foo", args = [ Var "Y" ] }) ] })

fact'goal :: [Goal]
fact'goal = parse'query "fact(s(s(s(s(s(s(s(z))))))), F)." -- fact 7
-- fact'goal = parse'query "fact(s(s(s(s(s(s(s(s(z)))))))), F)." -- fact 8 -- this is toooooo much!
-- fact'goal = parse'query "fact(s(s(s(s(s(z))))), F)." -- fact 5


init'state :: [Goal] -> State
init'state goals =  State { base = fact'base
                          , vars = free'vars'in'query goals
                          , backtracking'stack = []
                          , goal'stack = goals
                          , position = 0
                          -- , environment = (Map.empty, Map.empty)
                          , query'vars = Map.fromList q'vars
                          , counter = 0 }
  where
    free'names :: [String]
    free'names = Set.toList (free'vars'in'query goals)

    free'vars = map Var free'names

    q'vars = zip free'names free'vars


main :: IO ()
main = do
  -- putStrLn "Minolog - implementation of simple logic programming language."
  -- putStr "?- "
  -- hFlush stdout
  -- str <- getLine
  let goals = fact'goal
      state = init'state goals
  try'to'prove state
  putStrLn "Bye!"
-- main = do
--   putStrLn "Minolog - implementation of simple logic programming language."
--   putStr "?- "
--   hFlush stdout
--   str <- getLine
--   let goals = parse'query str
--       state = init'state goals
--   try'to'prove state
--   putStrLn "Bye!"


try'to'prove :: State -> IO ()
try'to'prove state = do
  case step state of
    Succeeded s -> do
      case step s of
        Redoing state' -> do
          putStrLn "Success and redoing"
          print $! query'vars s
          -- print $! environment s
          -- putStrLn (show'assignments s)
          -- ask user if they want to backtrack, if yes, then ->
          -- putStrLn ";"
          -- getLine
          -- try'to'prove state'
          -- if no, then -> putStrLn "."

        Done -> do
          putStrLn "Success and done"
          print $! query'vars s
          -- putStrLn $! (show'assignments s) ++ "."

        _ -> error "should never happen"

      -- TODO: wait for the interaction
      -- to know whether to attempt backtracking.
      -- I should change the step, so that the first two equations are not there
      -- another function would do that for me.
      -- that would allow me to sort of re-charge the state
      -- without misleadingly calling `step` or `try'to'prove`
      {-  Maybe it is not misleading. Maybe keeping the step's pattern
          matching exhaustive is worth it.  -}
      -- that function would either set me up for backtracking
      -- that would be signalized by `Redoing`
      -- or it would recognize that there is no way to backtrack
      -- so that would be signalized by `Done`.
      -- This would have the nice property of me knowing
      -- right away, whether I should hang for users's interaction
      -- or if I should just put `.` right away.

    Failed -> do
      putStrLn "false."

    Searching s -> do
      -- putStrLn "Searching ..."
      -- print s
      -- getLine -- just to wait for the enter
      try'to'prove s

    _ -> error "should never happen"


free'vars'in'query :: [Goal] -> Set.Set String
free'vars'in'query goals = foldl' (\ set g -> set `Set.union` free'vars'in'goal g) Set.empty goals


free'vars'in'goal :: Goal -> Set.Set String
free'vars'in'goal (Call fun) = free'vars'in'functor fun
free'vars'in'goal (Unify val'l val'r) = Set.union (free'vars'in'val val'l) (free'vars'in'val val'r)


free'vars'in'functor :: Functor -> Set.Set String
free'vars'in'functor Fun{ args } = foldl' (\ set g -> set `Set.union` free'vars'in'val g) Set.empty args


free'vars'in'val :: Value -> Set.Set String
free'vars'in'val (Var name) = Set.singleton name
free'vars'in'val (Atom name) = Set.empty
free'vars'in'val (Struct fun) = free'vars'in'functor fun
free'vars'in'val Wildcard = Set.empty
