module Main (main) where

import Chapter6

main :: IO ()
main = do
  --testFoldr
  --testFoldl
  --testInfFoldr
  -- print (duplicateOdds [1,2,3,4,5,6,7,8,9])
  -- print ( permutationsStartWith 'A' "DTsngzswarakljdlAölkjAkjfasdjfls")
  let info = [(1, 1), (1, 2), (4, 4), (4, 5), (8, 3), (3, 8), (4, 5), (5, 7), (20, 20), (100, 100)] :: [(Double, Double)]
  -- print $ kMeans initializeSample 2 info 0.001
  -- print $ initializeSample 2 info
  -- print $ clusterAssignmentPhase (initializeSample 2 info) info
  -- print $ newCentroidPhase (clusterAssignmentPhase (initializeSample 2 info) info)
  -- print $ map snd (newCentroidPhase (clusterAssignmentPhase (initializeSample 2 info) info))
  -- print $ printPerson
  -- print $ _lNameLens (PersonLens "Test3" "Test4" Female)
  -- print $ E.view fNameLens (PersonLens "Test3" "Test4" Male)
  -- let client = IndividualLens 3 (PersonLens "Test5" "Test6" Male)
  -- print $ client P.^. personLens.fNameLens
  print $ kMeansLens initializeSample 2 info 0.001
  print $ kMeansStateComb initializeSample 2 info 0.001
  print $ "Purchase Value 10 " ++ show (purchaseValue' 10)
  print $ purchaseValueDo 20
  print $ "mMeansStateDo : " ++ show (kMeansStateDo initializeSample 2 info 0.001)
  print $ "mMeansStateDoRunState : " ++ show (kMeansStateDoRunState initializeSample 3 info 0.001)
  print $ "mMeansStateDoLensRunState : " ++ show (kMeansStateDoLensRunState initializeSample 3 info 0.001)
  print $ "mMeansRWS : " ++ show (kMeansRWS initializeSample 2 info 0.001)
  print $ "mMeansST : " ++ show (kMeansST initializeSample 2 info 0.001)
  print "END"

