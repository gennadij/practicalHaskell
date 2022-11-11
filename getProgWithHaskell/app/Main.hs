module Main (main) where

import Lib (__select, __where)

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