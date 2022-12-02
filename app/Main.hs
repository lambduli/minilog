module Main where

import Data.Map.Strict qualified as Map


import AST.Term
import AST.Goal
import AST.Predicate

import Evaluate.Step
import Evaluate.State



init'base :: [Predicate]
init'base = [ Fact (Fun{ name = "id", args = [ Var "I", Var "I" ] })
            , Fact (Fun{ name = "foo", args = [ Var "X", Atom "thing" ] })
            ]


goal :: Goal
-- goal = Prove (Call (Fun{ name = "foo", args = [ Atom "something", Var "Y" ] }))
goal = Prove (Call (Fun{ name = "id", args = [ Var "Y", Struct (Fun{ name = "foo", args = [ Var "Y" ] }) ] }))


init'state :: State
init'state = State  { base = init'base
                    , backtracking'stack = []
                    , goal'stack = [goal]
                    , position = 0
                    , environment = (Map.empty, Map.empty)
                    , counter = 0 }


main :: IO ()
main = putStrLn "Hello, Haskell!"
