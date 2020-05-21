module Chapter09.GoodKingMarkov where

import Control.Monad.Trans.State
import System.Random

data Island =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  deriving (Eq, Show, Enum, Bounded)


next :: Island -> Island
next current
  | current == maxBound = minBound
  | otherwise = succ current

prev :: Island -> Island
prev current
  | current == minBound = maxBound
  | otherwise = pred current
 
data Move = Clockwise | Counterclockwise
  deriving (Eq, Show, Enum, Bounded)
  
instance Random Move where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g =
    randomR (minBound, maxBound) g
