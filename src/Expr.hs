module Expr
    ( Expr (..)
    ) where

-- | AST for simple arithmetic expressions
data Expr
  = Plus Expr Expr -- ^ Addition
  | Times Expr Expr -- ^ Multiplication
  | Int Int -- ^ Integer literal
  deriving (Eq, Show)
