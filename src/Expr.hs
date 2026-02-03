module Expr
    ( Expr (..)
    ) where

-- | AST for simple arithmetic expressions
data Expr
  = Plus Expr Expr -- ^ Addition
  | Minus Expr Expr -- ^ Subtraction
  | Times Expr Expr -- ^ Multiplication
  | Div Expr Expr -- ^ Division
  | Int Int -- ^ Integer literal
  deriving (Eq, Show)
