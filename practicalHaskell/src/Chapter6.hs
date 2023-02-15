{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE TemplateHaskell#-}
module Chapter6 where

import Data.List
import qualified Data.Map as M
import Lens.Micro.Platform
import OwnData
import Data.Char
import Control.Monad.State

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
-- =================
-- = Lenses        =
-- =================

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateLens e v
initializeState i n _points _threshold = KMeansStateLens (i n _points) _points (1.0 / 0.0) _threshold 0

clusterAssignmentPhaseLens :: (Vector v, Vectorizable e v) => KMeansStateLens e v -> M.Map v [e]
clusterAssignmentPhaseLens state =
  let p = state ^. points
      c = state ^. centroids
      initialMap = M.fromList $ zip c (repeat [])
      -- M.adjust (p:) (minimumBy (compareDistance p) _centroids) m
  in foldr (\p' m -> M.adjust (p' :) (minimumBy (compareDistance p') c) m) initialMap p
  where compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

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
-- ======================================================================================================================

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

purchaseByClientId :: Integer -> [Integer]
purchaseByClientId _ = [1, 2, 3]

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId _ = Just 3

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId _ = Just 4

priceByProductId :: Integer -> Maybe Double
priceByProductId _ = Just 5.0

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
  productIdByPurchaseId purchaseId    `thenDo` (\productId ->
  priceByProductId productId          `thenDo` (\price ->
  Just $ fromInteger n * price         )))

-- ==========================
-- Combinators for State
-- ==========================

type State_ s a = s -> (a, s)
thenDoState :: State_ s a -> (a -> State_ s b) -> State_ s b
thenDoState f g s  = let (resultOfF, stateAfterF) = f s in g resultOfF stateAfterF

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

kMeansStateComb :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansStateComb i k pts t = fst $ kMeansStateComb'' pts (initialStateComb i k pts t)

initialStateComb :: (Vector v, Vectorizable e v) =>
  (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateComb v
initialStateComb i k pts t = KMeansStateComb (i k pts ) t 0

kMeansStateComb' :: (Vector v, Vectorizable e v) => [e] -> State_ (KMeansStateComb v) [v]
kMeansStateComb' points =
  (\s -> (centroids__ s ,s ))                               `thenDoState` (\prevCentrs   ->
  (\s -> (clusterAssignmentPhase prevCentrs points , s))    `thenDoState` (\assignments  ->
  (\s -> (newCentroids assignments, s))                     `thenDoState` (\newCentrs    ->
  (\s -> ((), s {centroids__ = newCentrs}))                 `thenDoState` (\_            ->
  (\s -> ((), s {steps__ = steps__ s + 1}))                 `thenDoState` (\_            ->
  (\s -> (threshold__ s, s))                                `thenDoState` (\t            ->
  (\s -> (sum $ zipWith distance prevCentrs newCentrs, s))  `thenDoState` (\err          ->
  if err < t then (\s -> (newCentrs, s)) else (kMeansStateComb' points) )))))))

remain_ :: a -> (s -> (a, s))
remain_ x = \s -> (x, s)
access_ :: (s -> a) -> (s -> (a, s))
access_ f = \s -> (f s , s)

modify_ :: (s -> s) -> (s -> ((), s))
modify_ f = \s -> ((), f s)

kMeansStateComb'' :: (Vector v, Vectorizable e v) => [e] -> State_ (KMeansStateComb v) [v]
kMeansStateComb'' points =
  access_ centroids__                                        `thenDoState` (\prevCentrs   ->
  remain_ (clusterAssignmentPhase prevCentrs points)          `thenDoState` (\assignments  ->
  remain_ (newCentroids assignments)                         `thenDoState` (\newCentrs    ->
  modify_ (\s -> s {centroids__ = newCentrs})                `thenDoState` (\_            ->
  modify_ (\s -> s {steps__ = steps__ s + 1})                `thenDoState` (\_            ->
  access_ threshold__                                        `thenDoState` (\t            ->
  remain_ (sum $ zipWith distance prevCentrs newCentrs)      `thenDoState` (\err          ->
  if err < t then remain_ newCentrs else kMeansStateComb' points )))))))

-- ============================
-- Dissecting the Combinators
-- ============================

purchaseValue' :: Integer -> Maybe Double
purchaseValue' purchaseId =
  numberItemsByPurchaseId purchaseId   >>= (\n ->
  productIdByPurchaseId purchaseId     >>= (\productId ->
  priceByProductId productId           >>= (\price ->
  Just $ fromInteger n * price         )))

-- ============================
-- do Notation
-- ============================

purchaseValueDo :: Integer -> Maybe Double
purchaseValueDo purchaseId = do
  n            <- numberItemsByPurchaseId purchaseId
  productId    <- productIdByPurchaseId purchaseId
  price        <- priceByProductId productId
  return $ fromInteger n * price

kMeansStateDo' :: (Vector v, Vectorizable e v) => [e] -> State(KMeansStateComb v ) [v]
kMeansStateDo' points = do
  prevCentrs <- gets centroids__
  let assignments = clusterAssignmentPhase prevCentrs points
      newCentrs   = newCentroids assignments
  modify (\s -> s {centroids__ = newCentrs})
  modify (\s -> s {steps__ = steps__ s + 1})
  t <- fmap threshold__ get
  let err = sum $ zipWith distance prevCentrs newCentrs
  if err < t then return newCentrs else kMeansStateDo' points

kMeansStateDo :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansStateDo i k pts t = evalState (kMeansStateDo' pts) (initialStateComb i k pts t)

kMeansStateDoRunState :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> ([v], KMeansStateComb v)
kMeansStateDoRunState i k pts t = runState (kMeansStateDo' pts) (initialStateComb i k pts t)