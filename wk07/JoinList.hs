
module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
-- How to append two JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

-- Gets the annotation at the root of a Join list
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- find the index of a list
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- Convert JoinList to list ignoring annotations
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 2
-- Implementing fast indexing into a JoinList

-- Helper function  calculate the size of a join list
sz :: (Sized b, Monoid b)  => JoinList b a -> Int
sz = getSize . size . tag

-- finding the index of a join list in O(log n)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ i _  | i < 0     = Nothing
indexJ i jl | i > sz jl = Nothing
indexJ i (Single _ a)   = Just a
indexJ i (Append _ left right)
  | i < sz left = indexJ i left
  | otherwise   = indexJ (i - sz left) right

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty        = Empty
dropJ n jl | n <= 0  = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append _ left right)
  | n < sz left = (dropJ n left) +++ right
  | otherwise   = dropJ (n - sz left) right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty           = Empty
takeJ n jl | n <= 0     = Empty
takeJ n jl | n > sz jl  = jl
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ left right)
  | n < sz left = takeJ n left
  | otherwise   = left +++ takeJ (n - sz left) right

-- Exercise 3




