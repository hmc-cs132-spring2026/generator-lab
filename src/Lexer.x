{
module Lexer (tokenize) where

import Tokens (Token(..))
}

%wrapper "basic"

-- Token definitions
tokens :-
  $white+  ; -- ignore whitespace
  "+"      {\_ -> TokenPlus}
  "-"      {\_ -> TokenMinus}
  "*"      {\_ -> TokenTimes}
  "/"      {\_ -> TokenDiv}
  "("      {\_ -> TokenLParen}
  ")"      {\_ -> TokenRParen}
  [0-9]+   {\s -> TokenInt (read s)}

{

tokenize :: String -> [Token]
tokenize = alexScanTokens

}