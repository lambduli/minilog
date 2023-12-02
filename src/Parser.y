{
{-# LANGUAGE FlexibleContexts #-}

module Parser ( parse'base, parse'query ) where

import Control.Monad.Except ( throwError )
import Control.Monad.State
import Data.Either.Extra ( mapRight )

import Token ( Token )
import Token qualified

import Lexer

import Term

}


%name parseBase Base
%name parseBody Body 

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { Token.EOF }

%errorhandlertype explist
%error { parseError }

%token
  VAR     { Token.Var $$ }

  ATOM    { Token.Atom $$ }

  ','     { Token.Comma }
  '.'     { Token.Period }
  ':-'    { Token.If }
  '='     { Token.Equal }
  '('     { Token.Paren'Open }
  ')'     { Token.Paren'Close }
  '_'     { Token.Underscore }

%%


Base          ::  { [Predicate] }
              :   Predicates                { $1 }


Predicates    ::  { [Predicate] }
              :   Predicate                 { [ $1 ] }
              |   Predicate Predicates      { $1 : $2 }


Predicate     ::  { Predicate }
              :   Struct '.'               { Fact $1 }
              |   Struct ':-' Body         { $1 :- $3 }


Body          ::  { [Goal] }
              :   Goals '.'                 { $1 }


Struct        ::  { Struct }
              :   ATOM '(' Terms ')'        { Struct{ name = $1, args = $3 } }


Terms         ::  { [Term] }
              :   Term                      { [ $1 ] }
              |   Term ',' Terms            { $1 : $3 }

Term          ::  { Term }
              :   VAR                       { Var $1 }
              |   ATOM                      { Atom $1 }
              |   Struct                    { Compound $1 }
              |   '_'                       { Wildcard }


Goals         ::  { [Goal] }
              :   Goal                      { [ $1 ] }
              |   Goal ',' Goals            { $1 : $3 }


Goal          ::  { Goal }
              :   Struct                    { Call $1 }
              |   Term '=' Term             { Unify $1 $3 }

{

parse'base :: String -> Either (String, Int) [Predicate]
parse'base source = mapRight fst $! eval'parser parseBase source


parse'query :: String -> Either (String, Int) [Goal]
parse'query source = mapRight fst $! eval'parser parseBody source


parseError _ = do
  col'no <- gets (ai'col'no . lexer'input)
  l'no <- gets (ai'line'no . lexer'input)
  last'char <- gets (ai'last'char . lexer'input)
  state <- get
  throwError ("Parse error near character `" ++ [last'char] ++ "' on line " ++ show l'no ++ ", column " ++ show col'no ++ ".", col'no)

}