{
module Parser ( parse'base, parse'query ) where

import Prelude hiding ( Functor )

import Control.Monad.Error hiding ( Functor )
import Control.Monad.State hiding ( Functor )

import Lexer ( lexer, eval'parser, Lexer(..) )
import Token ( Token )
import Token qualified as Token
import Term ( Value(..), Functor(..), Predicate(..), Goal(..) )

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
              :   Functor '.'               { Fact $1 }
              |   Functor ':-' Body         { $1 :- $3 }


Body          ::  { [Goal] }
              :   Goals '.'                 { $1 }


Functor       ::  { Functor }
              :   ATOM '(' Patterns ')'     { Fun{ name = $1, args = $3 } }


Patterns      ::  { [Value] }
              :   Pattern                   { [ $1 ] }
              |   Pattern ',' Patterns      { $1 : $3 }

Pattern       ::  { Value }
              :   VAR                       { Var $1 }
              |   ATOM                      { Atom $1 }
              |   Functor                   { Struct $1 }
              |   '_'                       { Wildcard }


Goals         ::  { [Goal] }
              :   Goal                      { [ $1 ] }
              |   Goal ',' Goals            { $1 : $3 }


Goal          ::  { Goal }
              :   Functor                   { Call $1 }
              |   Pattern '=' Pattern       { Unify $1 $3 }

{

parse'base :: String -> [Predicate]
parse'base source = eval'parser parseBase source


parse'query :: String -> [Goal]
parse'query source = eval'parser parseBody source


parseError _ = do
  -- col'no <- gets (inpColumn . lexerInput)
  -- l'no <- gets (inpLine . lexerInput)
  -- state <- get
  error $ "Parse error on line " -- ++ show l'no ++ ", column " ++ show col'no ++ "." ++ "  " ++ show state
}