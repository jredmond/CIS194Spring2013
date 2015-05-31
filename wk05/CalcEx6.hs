{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Expr
import qualified Data.Map as M

-- Exercise 6
-- Add new feature: expressions with variables
data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

class HasVars a where
    var :: String -> a

instance HasVars VarExprT where
    var = Var

type EvalVarMap = (M.Map String Integer -> Maybe Integer)

-- M.lookup :: Ord k => k -> Map k a -> Maybe a

instance HasVars EvalVarMap where
    var = M.lookup

-- add and mul operate on two Maybe's and we want to return a single Maybe
-- abstract out this functionality
myLift :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
myLift _ Nothing _         = Nothing
myLift _ _ Nothing         = Nothing
myLift f (Just x) (Just y) = Just (f x y)

-- Operate on the maybe integers and pass through the map
instance Expr EvalVarMap where
    lit i   = (\_ -> Just i) -- Ignore the map when no need for a lookup
    add x y = (\m -> myLift (+) (x m) (y m))
    mul x y = (\m -> myLift (*) (x m) (y m))

withVars :: [(String, Integer)] -> EvalVarMap -> Maybe Integer
withVars vars expr = expr $ M.fromList vars