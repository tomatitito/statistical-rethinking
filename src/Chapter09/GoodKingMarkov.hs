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
  
data Direction = Clockwise | Counterclockwise
  deriving (Eq, Show, Enum, Bounded)

instance Random Direction where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

clockwise :: Island -> Island
clockwise current
  | current == maxBound = minBound
  | otherwise = succ current

counterclockwise :: Island -> Island
counterclockwise current
  | current == minBound = maxBound
  | otherwise = pred current

propose :: Island -> Direction -> Island
propose current to =
  case to of
    Clockwise -> clockwise current
    Counterclockwise -> counterclockwise current

randomDirection' :: State StdGen Direction
randomDirection' = state random -- (mkStdGen 0)

randomDirection :: Direction
randomDirection = evalState randomDirection' (mkStdGen 0)

moveOrStay :: Int -> Island -> Island
moveOrStay seed current =
   let proposal = propose current randomDirection
       randUnifValue = evalState  (state random) (mkStdGen seed) :: Float
       probMove = fromIntegral (toInteger (fromEnum proposal) `div` fromIntegral (toInteger (fromEnum current)))
   in 
     if randUnifValue < probMove
       then proposal
       else current