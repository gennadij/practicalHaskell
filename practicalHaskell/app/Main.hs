module Main (main) where

import Chapter2
import Chapter3
import Chapter6

main :: IO ()
main = do
  --testFoldr
  --testFoldl
  --testInfFoldr
  -- print (duplicateOdds [1,2,3,4,5,6,7,8,9])
  -- print ( permutationsStartWith 'A' "DTsngzswarakljdlAölkjAkjfasdjfls")
  let info = [(1, 1), (1, 2), (4, 4), (4, 5), (8, 3), (3, 8), (4, 5), (10, 1)] :: [(Double, Double)]
  print $ kMeans initializeSample 2 info 0.001