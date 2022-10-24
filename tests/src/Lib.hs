module Lib
    ( 
      someFunc,
      runRobotBattle,
      runTS
    ) where

import Control.Monad.Writer
import System.Random 
import Control.Monad.State
import Control.Monad.Error
import Data.List
import GetProgWithHaskell.Unit2 
import GetProgWithHaskell.Unit3

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

runRobotBattle :: IO ()
runRobotBattle = do
  let killerRobot = robot ("Killer", 25, 200)
  let speedRobot = robot ("Speed", 20, 200)
  let softRobot = robot ("Soft", 15, 200)
  putStrLn (printRobot killerRobot)
  putStrLn (printRobot speedRobot)
  putStrLn (printRobot softRobot)
  let r1 = fight killerRobot speedRobot
  let r2 = fight speedRobot killerRobot
  let r3 = fight r1 r2
  let r4 = fight r2 r1
  let r5 = fight r3 r4
  let r6 = fight r4 r3
  putStrLn (printRobot r5)
  putStrLn (printRobot r6) 

  let fastRobot = robot ("speedy", 15, 40)
  let  slowRobot = robot ("slowpoke",20,30)

  let fastRobotRound1 = fight slowRobot fastRobot
  let slowRobotRound1 = fight fastRobot slowRobot
  let fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
  let slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
  let fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
  let slowRobotRound3 = fight fastRobotRound2 slowRobotRound2

  putStrLn (printRobot fastRobotRound3)
  putStrLn (printRobot slowRobotRound3)
  
  putStrLn "END"

runTS :: IO ()
runTS = do 
  let ts1 = fileToTS file1
  let ts2 = fileToTS file2
  let ts3 = fileToTS file3
  let ts4 = fileToTS file4

  let tsAll_1 = ts1 <> ts2 <> ts3 <> ts4
  let tsAll_2 = mconcat [ts1, ts2, ts3, ts4]
 
  print tsAll_1

  print tsAll_2
  
  let mean = meanTS tsAll_1

  print mean
  print (meanTS tsAll_1)
  print (meanTS tsAll_2) 

  putStrLn "END" 



