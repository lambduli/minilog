module Main where

import Data.Map.Strict qualified as Map


import System.IO ( hFlush, stdout )

import Term ( Value(..), Functor(..), Predicate(..), Goal(..) )

import Evaluate.Step ( step )
import Evaluate.State ( State(..), Action(..) )

import Parser ( parse'base, parse'query )

import Visualize




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


init'state :: [Goal] -> State
init'state goals =  State { base = fact'base
                          , vars = free'vars'in'query goals
                          , backtracking'stack = []
                          , goal'stack = goals
                          , position = 0
                          , environment = (Map.empty, Map.empty)
                          , counter = 0 }


main :: IO ()
main = do
  putStrLn "Minolog - implementation of simple logic programming language."
  putStr "?- "
  hFlush stdout
  str <- getLine
  let goals = parse'query str
      state = init'state goals
  try'to'prove state
  putStrLn "Bye!"


try'to'prove :: State -> IO ()
try'to'prove state = do
  case step state of
    Succeeded s -> do
      case step s of
        Redoing state' -> do
          -- putStrLn "Success and redoing"
          -- print $! environment s
          putStrLn (show'assignments s)
          -- ask user if they want to backtrack, if yes, then ->
          -- putStrLn ";"
          getLine
          try'to'prove state'
          -- if no, then -> putStrLn "."

        Done -> do
          putStrLn "Success and done"
          putStrLn $! (show'assignments s) ++ "."

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