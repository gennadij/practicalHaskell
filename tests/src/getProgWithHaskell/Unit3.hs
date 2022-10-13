module Unit3 () where

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