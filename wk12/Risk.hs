{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Control.Applicative
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = (updateCasualties bf . getCasualties) <$> dieBattle (currentSkirmish bf)

updateCasualties :: Battlefield -> (Int, Int) -> Battlefield
updateCasualties (Battlefield atk def) (x,y) = Battlefield (atk-x) (def-y)

getCasualties :: [(DieValue,DieValue)] -> (Int, Int)
getCasualties = (\(a,b) -> (length a, length b)) . partition (\pair -> fst pair <= snd pair)

dieBattle :: Battlefield -> Rand StdGen [(DieValue,DieValue)]
dieBattle bf = zip <$> dieList (attackers bf) <*> dieList (defenders bf)
  where
    dieList len = reverse . sort <$> dice len
    dice n = sequence (replicate n die)

currentSkirmish :: Battlefield -> Battlefield
currentSkirmish b
  | attackers b < 2 || defenders b < 1 = Battlefield 0 0
  | otherwise = Battlefield (min (attackers b - 1) 3) (min (defenders b) 2)

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf <= 1 || defenders bf <= 0 = return bf
  | otherwise                              = battle bf >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    battles <- replicateM numSamples $ invade bf
    let part = partition attackSucceeded battles
    return (fromIntegral (length $ fst part) / fromIntegral numSamples)
  where
    numSamples = 1000
    attackSucceeded (Battlefield att def) = att > def

main :: IO ()
main = do
  values <- evalRandIO (successProb (Battlefield 3 2))
  putStrLn (show values)