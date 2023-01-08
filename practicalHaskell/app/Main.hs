module Main (main) where

import Chapter2
import Chapter3

main :: IO ()
main = do
  --testFoldr
  --testFoldl
  --testInfFoldr
  print (duplicateOdds [1,2,3,4,5,6,7,8,9])
  -- print ( permutationsStartWith 'A' "DTsngzswarakljdlAölkjAkjfasdjfls")