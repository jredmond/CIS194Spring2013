{-# LANGUAGE FlexibleInstances #-}
-- For q6 where the instace of num was only partially defined
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.List

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Very slow
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
-- A more efficient implementation of fibs
-- Want only 0(n) addition operations rather than exponential running time
-- foldl' :: (a -> b -> a) -> a -> [b] -> a
-- Tried fold and iterate but had an issue with reversing and infinite list
fibs2 :: [Integer]
fibs2 = g (1,0) where g (a,b) = b : g (a+b,a)

-- Exercise 3
-- New data type for polymorphic streams (lists that must be infinite)
data Stream a = SCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (SCons x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
    show xs = "[" ++ (concat . intersperse "," . map show . take 20 . streamToList) xs ++ ",...]"

-- Exercise 4
-- Creating tools to work with our new data type stream
streamRepeat :: a -> Stream a
streamRepeat a = SCons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SCons x xs) = SCons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = SCons a (streamFromSeed f (f a))

-- Exercise 5
-- Lets create some streams
nats :: Stream Integer
nats = streamFromSeed (1+) 0

-- This function hangs if I use pattern matching on the ys term
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (SCons x xs) ys = SCons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams . streamToList . streamMap streamRepeat $ nats

-- Print out the ruler to display with the new lines use putStr(...) 
printRuler :: Int -> String
printRuler n = foldr1 (++) (createAllLines (take n (streamToList ruler)))
  where
    createAllLines xs = map (createLine xs) (reverse [1..maximum xs])
    createLine ys line = foldr (addStar line) "\n" ys
    addStar n x acc
      | x >= n    = '*' : acc
      | otherwise = ' ' : acc

-- Execise 6
-- Fibonacci numbers via geneating functions
-- Generating functions of the form a0 + a1x + a2x^2 + ..+ anx^n

x :: Stream Integer
x = SCons 0 (SCons 1 (streamRepeat 0))
-- x :: Stream Integer
-- x = streamMap isOne nats
--   where
--     isOne x | x == 1    = 1
--             | otherwise = 0

instance Num (Stream Integer) where
    fromInteger n = SCons n (streamRepeat 0)
    negate (SCons x xs) = SCons (-x) (negate xs)
    (+) (SCons x xs) (SCons y ys) = SCons (x+y) $ xs + ys
    (*) (SCons a as) s2@(SCons b bs) = SCons (a*b) $ streamMap (* a) bs + as * s2

instance Fractional (Stream Integer) where
    (/) (SCons a as) (SCons b bs) = q
      where q = SCons (a `div` b) $ streamMap (`div` b) (as - q * bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
-- Calculating the nth Fibonacci number in O(log n) time via matricies

-- Create a type for 2x2 matricies of Integers
data Matrix = Matrix Integer Integer Integer Integer
                deriving (Eq, Show)

instance Num Matrix where
    (*) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) = 
      Matrix (x1*y1+x2*y3) (x1*y2+x2*y4) (x3*y1+x4*y3) (x3*y2+x4*y4)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = mat2fib $ f^n
  where
    f = Matrix 1 1 1 0
    mat2fib (Matrix _ fn _ _) = fn









