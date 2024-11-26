module Main where

import Lib
import TicTacToeLogic2

main :: IO ()
main = do
  putStrLn $ "The game is beginning."
  let newBoard = replicate 9 Empty
  playRound X newBoard
