{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE TemplateHaskell#-}
module Chapter6 where

import Data.List
import qualified Data.Map as M
import Lens.Micro.Platform
import OwnData
import Data.Char

makeLenses ''ClientLens
makeLenses ''PersonLens
makeLenses ''TimeMachineLens
makeLenses ''KMeansStateLens


class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) =  sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid lst = let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
                     n      = fromIntegral $ length lst
                 in (u / n, v / n)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase _centroids _points =
  let initialMap = M.fromList $ zip _centroids (repeat [])
    in foldr (
        -- \p m -> let chosenC = minimumBy (compareDistance p) centroids in M.adjust (p:) chosenC m
        \p m -> M.adjust (p:) (minimumBy (compareDistance p) _centroids) m
        ) initialMap _points
  where compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)  

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop _centroids _threshold = foldr (\(x, y) s -> s + distance x y) 0.0 _centroids < _threshold

kMeans :: (Vector v, Vectorizable e v) 
  => (Int -> [e] -> [v]) -- initialization function
  -> Int                 -- number of centroids
  -> [e]                 -- the information
  -> Double              -- threshold
  -> [v]                 -- final centroids
kMeans i k _points = kMeans' (i k _points) _points

kMeans' :: (Vector v, Vectorizable e v) =>
  [v] -> [e] -> Double -> [v]
kMeans' _centroids _points _threshold =
  let assignments     = clusterAssignmentPhase _centroids _points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
  in if shouldStop oldNewCentroids _threshold
     then newCentroids
     else kMeans' newCentroids _points _threshold
     
initializeSample :: Int -> [e] -> [(Double,Double)]
initializeSample 0 _ = []
initializeSample n v = (fromIntegral n, fromIntegral n) : initializeSample (n - 1) v

printPerson :: String
printPerson = do
  let client = IndividualLens 3 (PersonLens "Test5" "Test6" Female)
  let clientNew = client & personLens.lNameLens .~ "Test7"
  --let clientUpdateId = clientNew & clientIdLens +~ 3
  let clientNameUpper = clientNew & personLens.lNameLens %~ (map toUpper)
  "Old : " ++ client ^. personLens.lNameLens ++ ", New :" ++ clientNew ^. personLens.lNameLens ++
    ", NewID : " ++ ", ClientName to upper : " ++
    clientNameUpper ^. personLens.lNameLens

-- ======================================================================================================================
initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateLens e v
initializeState i n _points _threshold = KMeansStateLens (i n _points) _points (1.0 / 0.0) _threshold 0

clusterAssignmentPhaseLens :: (Vector v, Vectorizable e v) => KMeansStateLens e v -> M.Map v [e]
clusterAssignmentPhaseLens = undefined -- See exercise 6.3

kMeansLens :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansLens i n _points _threshold = view centroids $ kMeansLens' (initializeState i n _points _threshold)

kMeansLens' :: (Vector v, Vectorizable e v) => KMeansStateLens e v -> KMeansStateLens e v
kMeansLens' state =
  let assignments = clusterAssignmentPhaseLens state
      state1      = state & centroids.traversed
                          %~ (\c -> centroid
                            $ fmap toVector
                            $ M.findWithDefault [] c assignments)
      state2      = state1 & e .~ sum (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3      = state2 & steps +~1
  in if state3 ^. e < state3 ^. threshold then state3
     else kMeansLens' state3

