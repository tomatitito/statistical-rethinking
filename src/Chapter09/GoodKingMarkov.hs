module Chapter09.GoodKingMarkov where

import           Control.Monad.Trans.State
import           RIO.Prelude               (ask, lift)
import           RIO.Prelude.Types         (ReaderT)
import           System.Random
import RIO (liftIO)

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

islandToFloat :: Island -> Float
islandToFloat island =
  case island of
    One   -> 1.0
    Two   -> 2.0
    Three -> 3.0
    Four  -> 4.0
    Five  -> 5.0
    Six   -> 6.0
    Seven -> 7.0
    Eight -> 8.0
    Nine  -> 9.0
    Ten   -> 10.0

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

randomDirection :: State StdGen Direction
randomDirection = state random -- (mkStdGen 0)

randomFloat :: State StdGen Float
randomFloat = state $ randomR (0, 1)

data Env =
 Env
 {
  nSamples  :: Int,
  seed      :: Int,
  chain     :: [Island],
  generator :: StdGen
  }

mkEnv :: Env
mkEnv = Env
  {
    nSamples = 50,
    seed = 42,
    chain = [Ten],
    generator = mkStdGen 42
  }

propose :: Island -> State StdGen Island
propose current = propose' current <$> randomDirection

propose' :: Island -> Direction -> Island
propose' current to =
  case to of
    Clockwise        -> clockwise current
    Counterclockwise -> counterclockwise current

decide :: Island -> Island -> State StdGen Island
decide current proposal = do
  rand <- randomFloat
  if rand < probMove
    then return proposal
    else return current
  where probMove = islandToFloat proposal / islandToFloat current :: Float

travel :: Env -> Env
travel appData
  | length (chain appData) == nSamples appData = appData
  | otherwise = travel $ appData {chain = decision:islands, generator = newGnrtr}

  where
    islands = chain appData
    current:_ = islands
    gnrtr = generator appData
    decide' = decide current
    decisionState = return current >>= propose >>= decide'
    (decision, newGnrtr) = runState decisionState gnrtr

travel' :: ReaderT Env IO [Island]
travel' = do
  env <- ask
  liftIO $ return $ chain env-- putStrLn "Jucheeeee"
