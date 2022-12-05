module Main where

import Data.List ( foldl', intercalate )
import Data.List.Extra ( trim )

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import System.IO ( hFlush, stdout, openFile, IOMode(ReadMode), hGetContents )


import Term ( Term(..), Struct(..), Predicate(..), Goal(..) )

import Evaluate.Step ( step )
import Evaluate.State ( State(..), Action(..) )

import Parser ( parse'base, parse'query )


empty'state :: State
empty'state = State { base = []
                    , query'vars = Map.empty
                    , backtracking'stack = []
                    , goal'stack = []
                    , position = 0
                    , counter = 0 }


set'goal :: [Goal] -> State -> State
set'goal goals state = state{ query'vars = Map.fromList q'vars
                            , backtracking'stack = []
                            , goal'stack = goals
                            , position = 0
                            , counter = 0 }
  where
    free'names :: [String]
    free'names = Set.toList (free'vars'in'query goals)

    free'vars = map Var free'names

    q'vars = zip free'names free'vars


load'base :: [Predicate] -> State -> State
load'base base state = state{ base = base
                            , query'vars = Map.empty
                            , backtracking'stack = []
                            , goal'stack = []
                            , position = 0
                            , counter = 0 }


main :: IO ()
main = do
  putStrLn "Minolog - implementation of simple logic programming language."
  repl empty'state
  putStrLn "Bye!"


repl :: State -> IO ()
repl old'state = do
  putStr "?- "
  hFlush stdout
  str <- getLine
  case str of
    ":q" -> return ()
    ":Q" -> return ()
    ':' : 'l' : 'o' : 'a' : 'd' : file'path -> do
      file'handle <- openFile (trim file'path) ReadMode
      file'content <- hGetContents file'handle
      let new'base = parse'base file'content
          new'state = load'base new'base old'state
      repl new'state

    _ -> do
      let goals = parse'query str
          new'state = set'goal goals old'state
      try'to'prove new'state

try'to'prove :: State -> IO ()
try'to'prove state = do
  case step state of
    Succeeded s -> do
      case step s of
        Redoing state' -> do
          putStrLn $! intercalate "\n" $! map (\ (k, v) -> k ++ " = " ++ show v) $! Map.toList (query'vars s)
          user'input <- getLine
          case user'input of
            ":next" -> do
              putStrLn ";"
              try'to'prove state'
            ":done" -> do
              putStrLn "."
              repl state'
            _ -> do
              -- putStrLn "I have no idea what that's supposed to mean. I am gonna backtrack anyway."
              putStrLn ";"
              try'to'prove state'

        Done -> do
          putStrLn $! intercalate "\n" (map (\ (k, v) -> k ++ " = " ++ show v) (Map.toList (query'vars s))) ++ " ."
          repl s

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
      repl state

    Searching s -> do
      try'to'prove s

    _ -> error "should never happen"


free'vars'in'query :: [Goal] -> Set.Set String
free'vars'in'query goals = foldl' (\ set g -> set `Set.union` free'vars'in'goal g) Set.empty goals


free'vars'in'goal :: Goal -> Set.Set String
free'vars'in'goal (Call fun) = free'vars'in'functor fun
free'vars'in'goal (Unify val'l val'r) = Set.union (free'vars'in'val val'l) (free'vars'in'val val'r)


free'vars'in'functor :: Struct -> Set.Set String
free'vars'in'functor Struct{ args } = foldl' (\ set g -> set `Set.union` free'vars'in'val g) Set.empty args


free'vars'in'val :: Term -> Set.Set String
free'vars'in'val (Var name) = Set.singleton name
free'vars'in'val (Atom _) = Set.empty
free'vars'in'val (Compound fun) = free'vars'in'functor fun
free'vars'in'val Wildcard = Set.empty
