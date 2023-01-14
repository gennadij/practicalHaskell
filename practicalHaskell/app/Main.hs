module Main (main) where

import Chapter2
import Chapter3
import Chapter6
import OwnData
import qualified Lens.Micro.Extras as E
import qualified Lens.Micro.Platform as P

main :: IO ()
main = do
  --testFoldr
  --testFoldl
  --testInfFoldr
  -- print (duplicateOdds [1,2,3,4,5,6,7,8,9])
  -- print ( permutationsStartWith 'A' "DTsngzswarakljdlAölkjAkjfasdjfls")
  let info = [(1, 1), (1, 2), (4, 4), (4, 5), (8, 3), (3, 8), (4, 5), (10, 1)] :: [(Double, Double)]
  --print $ kMeans initializeSample 2 info 0.001
  --print $ initializeSample 2 info
  --print $ clusterAssignmentPhase (initializeSample 2 info) info
  --print $ newCentroidPhase (clusterAssignmentPhase (initializeSample 2 info) info)
  --print $ map snd (newCentroidPhase (clusterAssignmentPhase (initializeSample 2 info) info))
  print $ printPerson
  --print $ _lNameLens (PersonLens "Test3" "Test4" Female)
  --print $ E.view fNameLens (PersonLens "Test3" "Test4" Male)
  --let client = IndividualLens 3 (PersonLens "Test5" "Test6" Male)
  --print $ client P.^. personLens.fNameLens
