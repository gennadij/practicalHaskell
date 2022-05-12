module Logic where 

import Data.Array as A
import System.Random (StdGen, randomR)
import Control.Monad.State

data GameState = GameState 
  {
    board :: A.Array TileIndex TileState
  , currentPlayer :: Player
  , generator :: StdGen
  }

data Player = XPlayer | OPlayer

data TileState = Empty | HasX | HasO deriving Eq

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

  tileForPlayer :: Player -> TileState
  tileForPlayer XPlayer = HasX
  tileForPlayer OPlayer = HasO