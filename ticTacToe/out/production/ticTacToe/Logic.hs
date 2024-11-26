module Logic where 

import Data.Array as A
import Data.Ix as I
import System.Random (StdGen, randomR)
import Control.Monad.State

data GameState = GameState 
  {
    board :: A.Array TileIndex TileState
  , currentPlayer :: Player
  , generator :: StdGen
  } deriving (Show)

data Player = XPlayer | OPlayer deriving (Show)

data TileState = Empty | HasX | HasO deriving (Show, Eq)

type TileIndex = (Int, Int)

chooseRandomMove :: State GameState TileIndex
chooseRandomMove = do 
  game <- get
  let openSpots = [fst pair | pair <- A.assocs (board game), snd pair == Empty]
  let gen = generator game
  let (i, gen') = randomR (0, length openSpots - 1) gen
  put $ game {generator = gen'}
  return $ openSpots !! i

applyMove :: TileIndex -> State GameState ()
applyMove i = do
  game <- get 
  let p = currentPlayer game
  let newBoard = board game A.// [(i, tileForPlayer p)]
  put $ game {currentPlayer = nextPlayer p, board = newBoard}

tileForPlayer :: Player -> TileState
tileForPlayer XPlayer = HasX
tileForPlayer OPlayer = HasO

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

resolveTurn :: State GameState Bool
resolveTurn = do 
  i <- chooseRandomMove
  applyMove i
  isGameDone

isGameDone :: State GameState Bool
isGameDone = do
  game <- get
  let openSpots = [fst pair | pair <- A.assocs (board game), snd pair == Empty]
  return $ length openSpots == 0

boardIndices :: [TileIndex]
boardIndices = I.range ((0,0), (2,2))

initialGameState :: StdGen -> GameState
initialGameState gen = 
  GameState (A.array (head boardIndices, last boardIndices) [(i,Empty) | i <- boardIndices]) 
  XPlayer
  gen

runGame :: State GameState Bool
runGame = do
  -- Step 1
  s1 <- chooseRandomMove
  applyMove s1
  -- Step 1
  s2 <- chooseRandomMove
  applyMove s2
  s3 <- chooseRandomMove
  applyMove s3
  s4 <- chooseRandomMove
  applyMove s4
  s5 <- chooseRandomMove
  applyMove s5
  s6 <- chooseRandomMove
  applyMove s6
  s7 <- chooseRandomMove
  applyMove s7
  s8 <- chooseRandomMove
  applyMove s8
  s9 <- chooseRandomMove
  applyMove s9
  isGameDone

