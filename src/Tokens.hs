module Tokens
    ( Token (..)
    ) where

-- | Tokens for a simple arithmetic expression language
data Token
  = TokenPlus -- ^ Plus sign
  | TokenMinus -- ^ Minus sign
  | TokenTimes -- ^ Multiplication sign
  | TokenDiv -- ^ Division sign
  | TokenLParen -- ^ Left parenthesis
  | TokenRParen -- ^ Close parenthesis
  | TokenInt Int -- ^ Integer literal
  deriving (Eq, Show)
