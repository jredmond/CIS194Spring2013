{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- Haskell instance declaration doesn't like type synonyms

module Calc where -- one module Calc can span multiple files

import Expr
import Parser
import StackVM

-- Exercise 5
-- Need a version of the calculator that emits assembly language for the 
-- new processor.
-- Operations are embodied in data type: StackExp
instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]  

compile :: String -> Maybe Program
compile = parseExp lit add mul
