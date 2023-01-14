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
clusterAssignmentPhase centroids points = 
  let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (
        -- \p m -> let chosenC = minimumBy (compareDistance p) centroids in M.adjust (p:) chosenC m
        \p m -> M.adjust (p:) (minimumBy (compareDistance p) centroids) m
        ) initialMap points
  where compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)  

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v) 
  => (Int -> [e] -> [v]) -- initialization function
  -> Int                 -- number of centroids
  -> [e]                 -- the information
  -> Double              -- threshold
  -> [v]                 -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v) =>
  [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold = 
  let assignments     = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold
     then newCentroids
     else kMeans' newCentroids points threshold
     
initializeSample :: Int -> [e] -> [(Double,Double)]
initializeSample 0 _ = []
initializeSample n v = (fromIntegral n, fromIntegral n) : initializeSample (n - 1) v

printPerson :: String
printPerson = do
  let client = IndividualLens 3 (PersonLens "Test5" "Test6" Female)
  let clientNew = client & personLens.lNameLens .~ "Test7"
  let clientUpdateId = clientNew & clientIdLens +~ 3
  let clientNameUpper = clientNew & personLens.lNameLens %~ (map toUpper)
  "Old : " ++ client ^. personLens.lNameLens ++ ", New :" ++ clientNew ^. personLens.lNameLens ++
    ", NewID : " ++ show (clientUpdateId ^. clientIdLens) ++ ", ClientName to upper : " ++
    clientNameUpper ^. personLens.lNameLens



