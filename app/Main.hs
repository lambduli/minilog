module Main where

import Data.Map.Strict qualified as Map


import Term ( Value(..), Functor(..), Predicate(..), Goal(..) )

import Evaluate.Step ( step )
import Evaluate.State ( State(..), Action(..) )



init'base :: [Predicate]
init'base = [ Fact (Fun{ name = "id", args = [ Var "I", Var "I" ] })
            , Fact (Fun{ name = "foo", args = [ Var "X", Atom "thing" ] })
            ]


goal :: Goal
-- goal = Prove (Call (Fun{ name = "foo", args = [ Atom "something", Var "Y" ] }))
goal = Call (Fun{ name = "id", args = [ Var "Y", Struct (Fun{ name = "foo", args = [ Var "Y" ] }) ] })


init'state :: State
init'state = State  { base = init'base
                    , backtracking'stack = []
                    , goal'stack = [goal]
                    , position = 0
                    , environment = (Map.empty, Map.empty)
                    , counter = 0 }


main :: IO ()
main = putStrLn "Hello, Haskell!"

try'to'prove :: State -> IO ()
try'to'prove state = do
  case step state of
    Succeeded s@State{ environment } -> do
      case step s of
        Redoing state' -> do
          print environment
          -- ask user if they want to backtrack, if yes, then ->
          try'to'prove state'
          -- if no, then -> putStrLn "."

        Done -> do
          print environment
          putStrLn "." -- TODO: put the dot on the same line as the assignments

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

    Searching s ->
      try'to'prove s

    _ -> error "should never happen"