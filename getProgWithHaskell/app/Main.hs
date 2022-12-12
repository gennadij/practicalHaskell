module Main (main) where

import Lib

main :: IO ()
main = do 
  -- runRobotBattle
  putStrLn "============================================"
  -- runTS
  putStrLn "============================================"
  -- runRobotPart
  putStrLn "============================================"
  putStrLn "SQL Like Queries"
  putStrLn "============================================"
  print __select
  print __where
  print __join
  print selectResult