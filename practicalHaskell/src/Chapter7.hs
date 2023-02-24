module Chapter7 where

import Control.Monad (replicateM)


brockenThreeJumps :: Int  -> [Int]
brockenThreeJumps wishYear = do
  jump1 <- [-1, 3, 5]
  jump2 <- [-1, 3, 5]
  jump3 <- [-1, 3, 5]
  return $ wishYear + jump1 + jump2 + jump3

brockenJumps :: Int -> Int -> [[Int]]
brockenJumps year jumps = replicateM jumps [-1, 3 , 5]