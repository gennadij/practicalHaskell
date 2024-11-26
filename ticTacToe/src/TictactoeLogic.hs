module TictactoeLogic where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import Data.List

data Player = X | O deriving (Show, Eq)

newtype Board = Board (Map (Int, Int) (Maybe Player))

type Line = [(Int, Int)]

getMark :: Board -> (Int, Int) -> Maybe Player
getMark (Board board) (x, y)
  | x < 0 || x > 2 || y < 0 || y > 2 = error "Invalid coordinates"
  | otherwise = board ! (x, y)

putMark :: Board -> Player -> (Int, Int) -> Maybe Board
putMark (Board board) player (x, y)
  | x < 0 || x > 2 || y < 0 || y > 2 = error  $ "Invalid coordinates" ++ show (x, y)
  | Data.Maybe.isJust (board ! (x, y)) = Nothing
  | otherwise = Just $ Board $ Map.insert (x, y) (Just player) board

initBoard :: Board
initBoard = Board $ Map.fromList [((x, y), Nothing) | x <- [0..2], y <- [0..2]]

emptySquares :: Board -> [(Int, Int)]
emptySquares (Board board) = [(x, y) | x <- [0..2], y <- [0..2], isNothing (board ! (x,y))]

instance Show Board where 
  show (Board board) =
    intercalate "\n- - - \n"
      [(intercalate "|" [prettyShow $ board ! (x, y) | y <- [0..2]]) | x <- [0..2]]
      where 
        prettyShow Nothing = " "
        prettyShow (Just X) = "X"
        prettyShow (Just O) = "O"
        
winningLines :: [Line]
winningLines = [ [(x, y) | x <- [0..2]] | y <- [0..2] ] ++ 
               [ [(x, y) | y <- [0..2]] | x <- [0..2] ] ++ 
               [ [(0, 0), (1, 1), (2, 2) ],
                 [(0, 2), (1, 1), (2, 0) ]]
lineWinner :: Board -> Line -> Maybe Player
lineWinner b l
  | all (== Just X) marks = Just X
  | all (== Just O) marks = Just O
  | otherwise = Nothing
  where
    marks = map (getMark b) l

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> x       = x
x       <|> Nothing = x
Just x  <|> Just y  = Just x

boardWinner :: Board  -> Maybe Player
boardWinner b = foldr ((<|>) . lineWinner b) Nothing winningLines

isDraw :: Board -> Bool
isDraw b = null (emptySquares b) && isNothing (boardWinner b)

