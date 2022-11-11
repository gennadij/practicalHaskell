module Main (main) where

import Lib (__select)

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