module Lib
    ( someFunc
    ) where

import Control.Monad.Writer
import System.Random 
import Control.Monad.State
import Control.Monad.Error
import Data.List

someFunc :: IO ()
someFunc = mapM_ putStrLn  $ snd$ runWriter (gcd' 78 24)

-- newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

-- instance Monoid (DiffList a) where 
--   mempty = DiffList (\xs -> [] ++ xs)
--   (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- toDiffList :: [a] -> DiffList a
-- toDiffList xs = DiffList(xs++)

-- fromDiffList :: DiffList a -> [a]
-- fromDiffList (DiffList f) = f []

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b 
  | b == 0 = do 
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show(a `mod` b)]
      gcd' b (a `mod` b)

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
  tell ["0"]
finalCountDown x = do
  finalCountDown (x-1)
  tell [show x]

addStuff :: [Char] -> [Char] 
addStuff = do
  a <- (++ "first Text")
  b <- (++ "second text")
  return (a ++ b)

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random 

threeCoins :: State StdGen (Bool,Bool,Bool, Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt
    d <- randomSt 
    return (a,b,c,d) 

-- finalCountDownDiffList :: Int -> Writer (DiffList String) ()
-- finalCountDownDiffList 0 = do
--   tell (toDiffList ["0"])
-- finalCountDownDiffList x = do
--   finalCountDownDiffList (x-1)
--   tell (toDiffList [show x])


-- RPM Calculator

rpn1 :: String -> Double  
rpn1 = head . foldl foldingFunction1 [] . words

foldingFunction1 :: [Double] -> String -> [Double]  
foldingFunction1 (x:y:ys) "*" = (x * y):ys  
foldingFunction1 (x:y:ys) "+" = (x + y):ys  
foldingFunction1 (x:y:ys) "-" = (y - x):ys  
foldingFunction1 xs numberString = read numberString:xs 

foldingTest :: [Double] -> String -> [Double]
foldingTest dList st = read st:dList

rpn :: String -> Maybe Double 
rpn st = do
  [res] <- foldM foldingFunction [] (words st)
  return res

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _         -> Nothing 

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "+" = return ((x + y ) : ys)
foldingFunction (x:y:ys) "*" = return ((x * y ) : ys)
foldingFunction (x:y:ys) "-" = return((x - y ) : ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)
