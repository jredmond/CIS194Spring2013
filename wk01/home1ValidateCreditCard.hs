-- CIS194 Homework 1 (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)

-- toDigits 1234 == [1,2,3,4]
-- toDigitsRev 1234 == [4,3,2,1]
-- toDigits 0 == []
-- toDigits (-17) == []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] = [1,4,3]

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []       = []
doubleEveryOther' [x]      = [x]
doubleEveryOther' (x:y:zs) = x : (2 * y) : doubleEveryOther' zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOther' (reverse xs))

-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:ys) = sum (toDigits x) + sumDigits ys

-- validate 4012888888881881 = true
-- validate 4012888888881882 = false

validate :: Integer -> Bool
validate cc = sumDigits (doubleEveryOther (toDigits cc)) `mod` 10 == 0