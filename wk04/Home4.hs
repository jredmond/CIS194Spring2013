{-# OPTIONS_GHC -Wall #-}
module Home4 where

import Data.List
-- Exercise 1 Wholemeal programming
-- Reimplement fun1 and fun2 in more idiomatic Haskell style
-- Use wholemeal prgramming practices: breaking each function into a pipeline
--   of incremental transformations
-- Hint: look into iterate and takeWhile from Prelude (try Hayoo)
{-
fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs
-}
fun1 :: [Integer] -> Integer
fun1 xs = foldl (\acc cur -> (cur - 2) * acc) 1 $ filter even xs
-- Alt: (\acc -> (*) acc . flip (-) 2)

{-
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
-}
fun2 :: Integer -> Integer
fun2 num = sum $ filter even $ takeWhile (/=1) $ iterate fun2' num
  where
    fun2' n | even n    = n `div` 2
            | otherwise = 3 * n + 1

-- Exercise 2 Folding with trees
-- Write foldTree which generates a balanced binary tree
-- use foldr
-- balanced binary tree: hight of left and right subtrees 
-- differ by no more than one.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- foldr :: (elem -> Tree b -> Tree b) -> Tree b -> [a] -> Tree b
foldTree :: [a] -> Tree a
foldTree xs = foldr addToTree Leaf xs

addToTree :: a -> Tree a -> Tree a
addToTree a Leaf               = Node 0 Leaf a Leaf
addToTree a (Node h l b r)
  | getHeight l <= getHeight r = Node (h+1) (addToTree a l) b r
  | otherwise                  = Node h l b (addToTree a r)

getHeight :: Tree a -> Integer
getHeight Leaf                = 0
getHeight (Node height _ _ _) = height 

isBalanced :: Tree a -> Bool
isBalanced Leaf           = True
isBalanced (Node _ l _ r) = abs (getHeight l - getHeight r) <= 1 && isBalanced l && isBalanced r 

-- Exercise 3: More folds!
-- odd number of True
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = foldl' (\acc x -> if x then not acc else acc) False

-- Implement map as a fold in such a way that map' behaves identically to the
-- the standard map function
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- (Optional) Implement foldl using foldr (identical to foldl)
-- Hint: Study how the application of foldr and foldl work out
-- foldr (a -> b -> b) -> b -> [a] -> b
myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...
myFoldl f base xs = foldr (flip f) base $ reverse xs

-- Exercise 4: Finding primes
-- Sieve of Sundaram:
-- Start with a list of integers from 1 to n
-- Remove all numbers of the form i + j + 2ij
-- 1 <= i <= j and stop when above > n
-- The remaining numbers are doubled and incremented by 1
-- Gives all the odd primes (excludes 2) below 2n + 2 
-- Given an integer n generate all the odd prime numbers up to 2n + 2
-- sieveSundaram :: Integer -> [Integer]
-- First ideas: sieveSundaram n = take 2 $ iterate (\i -> takeWhile (\(a,b) -> a + b + 2*a*b <= n) (cartProd [i] [i..])) 1
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let del = sieveSundaramDelete n in
    2:[2*x+1 | x <- [1..n], not (x `elem` del)]

-- Exclusion list
sieveSundaramDelete' :: Integer -> [Integer]
sieveSundaramDelete' n = [i+j+2*i*j | i <- [1..n], j <- [i..n],i+j+2*i*j<=n]
-- Speeding things up, the smallest j can be is i
-- Therefore smallest i+j+2ij is 2i^2+2i
-- To cull unnessessary i we only need:
-- 2i^2+2i <= n
-- Ok to go over n as that doesn't break correctness
-- Can simplify to 2i^2 <= n
-- Can be espressed as i <= sqrt(n/2)
-- For j again we want i+j+2ij <= n
-- Rearrange: 
-- (2i+1)j <= n-i
-- j <= (n-i)/(2i+1)
sieveSundaramDelete :: Integer -> [Integer]
sieveSundaramDelete n = [i+j+2*i*j | 
                         let n' = fromIntegral n :: Double,
                         i <- [1..floor (sqrt (n' / 2))],
                         let i' = fromIntegral i :: Double,
                         j <- [i..floor ((n'-i')/(2*i'+1))]
                        ]

-- i optimisation
-- i <- [1..floor (((sqrt (2 * n' + 1)) - 1) / 2)],


-- cartProd [1,2] ['a','b'] == [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- Written using a list comprehension which we haven't talked about in class
-- (but feel free to research them)
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]









