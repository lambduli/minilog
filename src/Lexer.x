{
module Lexer ( lexer, read'token, eval'parser, Lexer(..), Lexer'State(..), AlexInput(..) ) where

import Control.Monad.State ( MonadState(get, put), gets, StateT( runStateT ), State )
import Control.Monad.Except ( Except, runExcept, throwError )

import Data.Word ( Word8 )
import Data.Char ( ord )
import Data.List ( uncons )

import Token ( Token )
import Token qualified as Token

}


$upper                = [A-Z]

$lower                = [a-z]

@variableident        = $upper+

@atomident            = $lower+

$space                = [\ \t\f\v\n]


minilog :-

$space+                 ;

"%".*\n                 ;

","                     { \_ -> token Token.Comma }

"."                     { \_ -> token Token.Period }

":-"                    { \_ -> token Token.If }

"="                     { \_ -> token Token.Equal }

"("                     { \_ -> token Token.Paren'Open }

")"                     { \_ -> token Token.Paren'Close }

"_"                     { \_ -> token Token.Underscore }

@variableident          { emit Token.Var }

@atomident              { emit Token.Atom }


{

lexer :: (Token -> Lexer a) -> Lexer a
lexer cont = read'token >>= cont


read'token :: Lexer Token
read'token = do
  s <- get
  case alexScan (lexer'input s) 0 of
    AlexEOF -> return Token.EOF

    AlexError inp' ->
      throwError ("Lexical error on line " ++ (show $! ai'line'no inp') ++ " and column " ++ (show $! ai'col'no inp'), ai'col'no inp')
    
    AlexSkip inp' _ -> do
      put s{ lexer'input = inp' }
      read'token
    
    AlexToken inp' n act -> do
      let (Input{ ai'input = buf }) = lexer'input s
      put s{ lexer'input = inp' }
      act (take n buf)


-- The functions that must be provided to Alex's basic interface
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@Input{ ai'input }
  = advance <$> uncons ai'input
    where
      advance :: (Char, String) -> (Word8, AlexInput)
      advance ('\n', rest)
        = ( fromIntegral (ord '\n')
          , Input { ai'line'no    = ai'line'no input + 1
                  , ai'col'no     = 1
                  , ai'last'char  = '\n'
                  , ai'input      = rest } )
      advance (c, rest)
        = ( fromIntegral (ord c)
          , Input { ai'line'no    = ai'line'no input
                  , ai'col'no     = ai'col'no input + 1
                  , ai'last'char  = c
                  , ai'input      = rest } )


token :: Token -> Lexer Token
token t = return t


emit :: (String -> Token) -> String -> Lexer Token
emit mk't str = return (mk't str)


get'line'no :: Lexer Int
get'line'no = gets (ai'line'no . lexer'input)


get'col'no :: Lexer Int
get'col'no = gets (ai'col'no . lexer'input)


eval'parser :: Lexer a -> String -> Either (String, Int) (a, Lexer'State)
eval'parser parser source = runExcept $! runStateT parser (initial'state source)


type Lexer a = StateT Lexer'State (Except (String, Int)) a


data AlexInput = Input
  { ai'line'no   :: !Int
  , ai'col'no    :: !Int
  , ai'last'char :: !Char
  , ai'input     :: String }
  deriving (Eq, Show)


data Lexer'State = Lexer'State
  { lexer'input :: !AlexInput }
  deriving (Eq, Show)


initial'state :: String -> Lexer'State
initial'state s = Lexer'State
  { lexer'input       = Input
                        { ai'line'no    = 1
                        , ai'col'no     = 1 
                        , ai'last'char  = '\n'
                        , ai'input      = s } }

}
