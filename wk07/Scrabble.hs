{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.Char

-- Exercise 3

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score l
  | hasLetter l "aeioulnstr" = Score 1
  | hasLetter l "dg"         = Score 2
  | hasLetter l "bcmp"       = Score 3
  | hasLetter l "fhvwy"      = Score 4
  | hasLetter l "k"          = Score 5
  | hasLetter l "jx"         = Score 8
  | hasLetter l "qz"         = Score 10
  | otherwise                = Score 0
  where
    hasLetter = elem . toLower

scoreString :: String -> Score
scoreString = sum . map score


