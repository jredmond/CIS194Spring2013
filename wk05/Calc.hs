module Calc where

import Expr
import ExprT
import Parser

-- domain-specific languages

-- Exercise 1
-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Exercise 2
-- Utilising parseExp from Parser.hs
-- evalStr evaluates expressions, not well formed return Nothing,
-- If well formed return just n
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                   Nothing   -> Nothing
                   Just expr -> Just (eval expr)
-- Alternatively
-- evalStr = fmap eval . parseExp Lit Add Mul

-- Exercise 3
-- Create a type class called Expr with three methods lit, add and mul which
-- parallel the constructors of ExprT
-- See: Expr.hs

-- Make an instance of Expr for the ExprT type
instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- constrain the type of the functions argument
-- calling this function provides context to the compiler for type inference
reify :: ExprT -> ExprT
reify = id -- identity function

-- Therefore reify $ {Expr} is equivalent to {Expr} :: ExprT

-- Exercise 4
-- Write 4 more instaces of Expr to suit the custom requirements
-- 3 of the instances work with integers, to provide different instances
-- we wrap two of those Integers in newtype wrapper. 
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit i = i > 0
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7




