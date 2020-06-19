module Chapter09.GoodKingMarkov where

import           Control.Monad.Trans.State
import           RIO                        (IORef, liftIO, modifyIORef,
                                             newIORef, readIORef, runReaderT,
                                             writeIORef)
import           RIO.Prelude                (ask)
import           RIO.Prelude.Types          (ReaderT)
import           System.Random

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

--newtype Chain = [Island]
type App = ReaderT Env IO

data Env =
 Env
 {
  nSamples  :: Int,
  seed      :: Int,
  chain     :: IORef [Island],
  generator :: IORef StdGen
  }

class HasChain a where
  getChain :: a -> IORef [Island]

--instance HasChain Chain where
--  getChain = id

instance HasChain Env where
  getChain = chain

class HasGenerator a where
  getGenerator :: a -> IORef StdGen

--instance HasGenerator (IO (IORef StdGen)) where
--  getGenerator = id

instance HasGenerator Env where
  getGenerator = generator

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

modifyChain :: (HasChain env) => env -> Island -> IO ()
modifyChain env decision = do
  let chainRef = getChain env
  chain <- readIORef chainRef
  writeIORef chainRef $ decision:chain

travel :: IO [Island]
travel = do
  initialChain <- newIORef [Ten]
  initialGnrtr <- newIORef $ mkStdGen 54
  let env = Env
        { nSamples = 42
        , seed = 42
        , chain = initialChain
        , generator = initialGnrtr
        }
  runReaderT
    recursiveTravel
    env

  readIORef $ getChain env

  where recursiveTravel :: (HasChain env, HasGenerator env) => ReaderT env IO ()
        recursiveTravel = do
          env <- ask
          let chainRef = getChain env
          chain <- liftIO $ readIORef chainRef
          let genRef = getGenerator env
          gen <- liftIO $ readIORef genRef
          if length chain == 42
            then return ()
            else do
              let decide' = decide (head chain)
              let decisionState = return (head chain) >>= propose >>= decide'
              let (decision, newGnrtr) = runState decisionState gen
              liftIO $ modifyChain env decision
              liftIO $ writeIORef genRef newGnrtr
              recursiveTravel

