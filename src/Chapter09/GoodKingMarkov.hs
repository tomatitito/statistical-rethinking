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


rollDie = evalState rollDie' (mkStdGen 0)
clockwise :: Island -> Island
clockwise current
  | current == maxBound = minBound
  | otherwise = succ current

counterclockwise :: Island -> Island
counterclockwise current
  | current == minBound = maxBound
  | otherwise = pred current

data Direction = Clockwise | Counterclockwise
  deriving (Eq, Show, Enum, Bounded)

instance Random Direction where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

propose :: Island -> Direction -> Island
propose current to =
  case to of
    Clockwise -> clockwise current
    Counterclockwise -> counterclockwise current

randomDirection' :: State StdGen Direction
randomDirection' = state $ random -- (mkStdGen 0)

randomDirection :: Direction
randomDirection = evalState randomDirection' (mkStdGen 0)

