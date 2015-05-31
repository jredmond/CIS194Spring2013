module Expr where

-- From exercise 3
-- type class that mirrors ExprT
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a