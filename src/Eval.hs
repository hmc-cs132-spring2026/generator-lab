module Eval
    ( eval
    ) where

import           Expr (Expr (..))

-- | Evaluate an arithmetic expression
eval :: Expr -> Int
eval (Int n)       = n
eval (Plus e1 e2)  = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
