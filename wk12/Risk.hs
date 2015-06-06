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
battle bf = do
    attackRolls <- replicateM (numAttackers bf) die
    defendRolls <- replicateM (numDefenders bf) die
    let pairedRolls = zip (reverse . sort $ attackRolls) (reverse . sort $ defendRolls)
    return $ foldl applySkirmish bf pairedRolls
  where
    numAttackers = min 3 . (subtract 1) . attackers
    numDefenders = min 2 . defenders

applySkirmish :: Battlefield -> (DieValue,DieValue) -> Battlefield
applySkirmish (Battlefield a d) (atkDie,defDie)
  | atkDie > defDie = Battlefield a (d-1)
  | otherwise       = Battlefield (a-1) d

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