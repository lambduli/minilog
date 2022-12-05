{
module Parser ( parse'base, parse'query ) where

import Control.Monad.Error
import Control.Monad.State

import Lexer ( lexer, eval'parser, Lexer(..) )
import Token ( Token )
import Token qualified as Token
import Term ( Term(..), Struct(..), Predicate(..), Goal(..) )

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


Struct       ::  { Struct }
              :   ATOM '(' Terms ')'        { Struct{ name = $1, args = $3 } }


Terms      ::  { [Term] }
              :   Term                      { [ $1 ] }
              |   Term ',' Terms            { $1 : $3 }

Term       ::  { Term }
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