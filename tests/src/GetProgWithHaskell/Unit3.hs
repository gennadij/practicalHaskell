module GetProgWithHaskell.Unit3 (
  meanTS,
  fileToTS,
  file1,
  file2,
  file3,
  file4,
  TS (..)
) where

import qualified Data.Map as Map
import Data.Maybe

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable e p = PTable e normolizedProbs
  where 
    normolizedProbs :: Probs
    normolizedProbs = map (/ totalProbs) p
    totalProbs :: Double
    totalProbs = sum p

showPair :: String -> Double -> String
showPair event prob = mconcat ["| ", event, " | ", show prob, " |\n"]

instance Show PTable where 
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func  newL1 cycledL1
  where nToAdd = length l2
        repetedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repetedL1
        cycledL1 = cycle l2

-- ############################################################################
-- Lesson 20
-- ############################################################################

data TS a = TS [Int] [Maybe a]

file1 :: [(Int,Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4)
        ,(4, 198.9), (5, 199.0), (6, 200.2)
        ,(9, 200.3), (10, 201.2), (12, 202.9)]
file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]
file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]
file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

createTS :: [Int] -> [a] -> TS a
createTS times values  = TS completeTimes extendedValues
  where completeTimes  = [minimum times .. maximum times]
        timeValueMap   = Map.fromList (zip times values)
        extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

showTVPairs :: Show a => Int -> Maybe a -> String
showTVPairs time (Just value) = mconcat ["| ", show time, " | ", show value, " |\n"]
showTVPairs time Nothing = mconcat ["| ", show time, " | ", "NA |\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows 
    where rows = zipWith showTVPairs times values

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair m (_, Nothing) = m
insertMaybePair m (key, Just value) = Map.insert key value m

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes      = mconcat [t1, t2]
        completeTimes  = [minimum  bothTimes .. maximum bothTimes]
        tvMap          = foldl insertMaybePair Map.empty (zip t1 v1)
        updateMap      = foldl insertMaybePair tvMap (zip t2 v2)
        combinedValues = map (\v -> Map.lookup v updateMap) completeTimes

instance Semigroup (TS a) where 
  (<>) = combineTS

instance Monoid (TS a) where
  mempty  = TS [] []
  mappend = (<>)

mean :: (Real a) => [a] -> Double
mean xs = total/count 
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ [])         = Nothing
meanTS (TS teams values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
  where justVals  = filter isJust values
        cleanVals = map fromJust justVals
        avg       = mean cleanVals

isNothingValue :: Maybe v -> Bool 
isNothingValue (Just v) = False
isNothingValue Nothing = True

tsToMap :: TS a -> Map.Map Int a
tsToMap (TS teams values) = foldl insertMaybePair Map.empty (zip teams values)

filterTSNothing :: TS a -> TS a
filterTSNothing (TS key value) = undefined
--  where filterValues m =  Map.delete k (Map k a)
           
