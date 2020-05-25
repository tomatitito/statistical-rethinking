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

propose' :: Island -> Direction -> Island
propose' current to =
  case to of
    Clockwise -> clockwise current
    Counterclockwise -> counterclockwise current

proposeHelperState :: Island -> Direction -> State StdGen Island
proposeHelperState current direction = do
  let proposal = propose' current direction
  return proposal

randomDirection' :: State StdGen Direction
randomDirection' = state random -- (mkStdGen 0)

randomDirection :: Direction
randomDirection = evalState randomDirection' (mkStdGen 0)

moveOrStay :: Int -> Island -> Island
moveOrStay seed current =
   let proposal = propose' current randomDirection
       randUnifValue = evalState  (state random) (mkStdGen seed) :: Float
       probMove = fromIntegral (toInteger (fromEnum proposal) `div` fromIntegral (toInteger (fromEnum current)))
   in 
     if randUnifValue < probMove
       then proposal
       else current

data MarkovData =
 MarkovData
 {
  seed :: Int,
  chain :: [Island],
  state' :: State StdGen Island
  }

moveOrStay'' :: Island -> State StdGen Island
moveOrStay'' current =
  proposeHelperState current randomDirection

--x = evalState (moveOrStay'' 42 Ten) (mkStdGen 43)

travel :: MarkovData -> MarkovData
travel dat =
  let islands = chain dat
      s = state' dat
      current = evalState s (mkStdGen (seed dat)) -- instead: get current from chain
      newState = moveOrStay'' current
      (newPosition, newGen) = runState newState (mkStdGen (seed dat))
  in MarkovData {seed = seed dat, chain = newPosition:islands, state' = newState}

x = chain $ travel MarkovData {seed = 42, chain = [], state' = proposeHelperState Ten Clockwise}

-- with MarkovData
-- propose an island (or rather, a direction)
-- decide to move or stay
-- write decision into MarkovData
-- update state in MarkovData
-- recurse

-- MarkovData needs to have info about recursion base case
-- travel function is recursive
-- logic is:
-- propose, decide, update

-- using bind :: m a -> a -> m b -> m b
--               State Island -> propose -> State Island
-- propose :: Island -> State StdGen Island
-- decide :: Island -> State StdGen Island (moveOrStay)
-- initial >>= propose
-- proposal >>= decide

propose :: Island -> State StdGen Island
propose current = do
  generator <- get
  let direction = evalState randomDirection' generator
  return $ propose' current direction
