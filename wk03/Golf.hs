{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Exercise 1 Hopscotch
-- First list same as input list, second contains every second element...
skips :: [a] -> [[a]]
skips [] = []
skips list = map (\n -> everyNth n list) [1..length list]

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n-1) xs of
                     (y:ys) -> y : everyNth n ys
                     []     -> []

-- Exercise 2 Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
    | y > x && y > z = y : localMaxima (y:z:rest)
    | otherwise      = localMaxima (y:z:rest)
localMaxima _ = []

-- Exercise 3 Histogram
histogram :: [Integer] -> String
histogram list = histogramBars (calcHistogram list)

histogramBars :: [Integer] -> String
histogramBars hist = foldr (++) "==========\n0123456789\n" (createAllLines hist)
  where
    createAllLines xs = map (createLine xs) (reverse [1..maximum xs])
    createLine ys line = foldr (addStar line) "\n" ys
    addStar n x acc
      | x >= n    = '*' : acc
      | otherwise = ' ' : acc

calcHistogram :: [Integer] -> [Integer]
calcHistogram xs = map (digitCount xs) [0..9]
  where
    digitCount ys n = toInteger $ length $ filter (==n) ys

{-
import Data.List
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x:y:z:_) <- tails xs, x < y && y > z]

skips :: [a] -> [[a]]
skips []   = []
skips list = skips' 1 (length list) list
  where 
    skips' :: Int -> Int -> [a] -> [[a]]
    skips' n len xs 
      | n > len   = []
      | otherwise = everyNth n xs : skips' (n+1) len xs


everyNth :: Int -> [a] -> [a]
everyNth 0 _  = []
everyNth 1 xs = xs
everyNth n xs = everyNth' 1 n xs 
  where
    everyNth' :: Int -> Int -> [a] -> [a]
    everyNth' _ _ [] = []
    everyNth' count target (y:ys)
      | count == target = y : everyNth' 1 target ys
      | otherwise       = everyNth' (count+1) target ys
-}



