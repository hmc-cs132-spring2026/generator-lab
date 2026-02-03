{
module Lexer (tokenize) where

import Tokens (Token(..))
}

%wrapper "basic"

-- Macros
$digit  = [0-9]

-- Token definitions
tokens :-
  $white+  ; -- ignore whitespace
  "+"      {\_ -> TokenPlus}
  "*"      {\_ -> TokenTimes}
  "("      {\_ -> TokenLParen}
  ")"      {\_ -> TokenRParen}
  [0-9]+   {\s -> TokenInt (read s)}

{

tokenize :: String -> [Token]
tokenize = alexScanTokens

}