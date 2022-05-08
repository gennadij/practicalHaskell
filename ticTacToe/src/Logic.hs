module Logic where 

import Data.Array as A
import System.Random (StdGen)

data GameState = GameState 
  {
    board :: A.Array TileIndex TileState
  , currentPlayer :: Player
  , generator :: StdGen
  }

data Player = XPlayer | OPlayer

data TileState = Empty | HasX | HasO deriving Eq

type TileIndex = (Int, Int)