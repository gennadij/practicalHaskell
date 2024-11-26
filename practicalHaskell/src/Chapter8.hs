module Chapter8 where

import Control.Monad.Par
import Control.DeepSeq

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                in oneFactor : (findFactors $ n `div` oneFactor)

-- findFactors_ :: Integer -> [Integer]
-- findFactors_ 1 = [1]
-- findFactors_ n = (findFactor n 2) : (findFactors(n `div` (findFactor n 2)))

findFactor :: Integer -> Integer ->  Integer
findFactor n m | n == m         = m
               | n `mod` m == 0 = m
               | otherwise      = findFactor n (m + 1)  
               
findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY  = findFactors y 
      _         = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)

-- #############################################################################

lookupPar :: (Eq a, NFData b) => IVar (Maybe b) -> a -> [(a, b)] -> Par()
lookupPar i _ []                     = put i Nothing
lookupPar i x ((k, v):r) | x == k    = put i $ Just v
                         | otherwise = lookupPar i x r

printEnvelop :: IVar (Maybe String) -> IVar String -> Par ()
printEnvelop clientV envV = do
  clientName <- get clientV
  case clientName of
    Nothing -> put envV "Unkown"
    Just n  -> put envV $ "To: " ++ n

printLetter :: IVar (Maybe String) -> IVar (Maybe String) -> IVar String -> Par ()
printLetter clientV productV letterV = do
  clientName  <- get clientV
  productName <- get productV
  case (clientName, productName) of
    (Nothing, Nothing) -> put letterV "Unknown"
    (Just n, Nothing)  -> put letterV $ n ++ "bought somethink"
    (Nothing, Just p)  -> put letterV $ "Some bought " ++ p
    (Just n, Just p)   -> put letterV $ n ++ " bought " ++ p

printTicket :: Int -> Int -> [(Int, String)] -> [(Int, String)] -> String
printTicket idC idP clients products = runPar $ do
  clientV <- new
  productV <- new
  fork $ lookupPar clientV idC clients
  fork $ lookupPar productV idP products
  envV <- new
  letterV <- new
  fork $ printEnvelop clientV envV
  fork $ printLetter clientV productV letterV
  envS <- get envV
  letterS <- get letterV
  return $ envS ++ "\n\n" ++letterS
