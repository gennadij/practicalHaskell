module Lib
    (
      runRobotBattle,
      runTS, 
      runRobotPart, 
      __select, 
      __where, 
      __join,
      selectResult
    ) where

import GetProgWithHaskell.Unit2 
import GetProgWithHaskell.Unit3
import GetProgWithHaskell.Unit5
import GetProgWithHaskell.Unit5Capstone


--someFunc :: IO ()
--someFunc = mapM_ putStrLn  $ snd$ runWriter (gcd' 78 24)

-- newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

-- instance Monoid (DiffList a) where 
--   mempty = DiffList (\xs -> [] ++ xs)
--   (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- toDiffList :: [a] -> DiffList a
-- toDiffList xs = DiffList(xs++)

-- fromDiffList :: DiffList a -> [a]
-- fromDiffList (DiffList f) = f []

-- gcd' :: Int -> Int -> Writer [String] Int
-- gcd' a b
--   | b == 0 = do
--      tell ["Finished with " ++ show a]
--      return a
--  | otherwise = do
--      tell [show a ++ " mod " ++ show b ++ " = " ++ show(a `mod` b)]
--      gcd' b (a `mod` b)

--finalCountDown :: Int -> Writer [String] ()
--finalCountDown 0 = do
--  tell ["0"]
--finalCountDown x = do
--  finalCountDown (x-1)
--  tell [show x]

--addStuff :: [Char] -> [Char] -
--addStuff = do
--  a <- (++ "first Text")
--  b <- (++ "second text")
--  return (a ++ b)

--randomSt :: (RandomGen g, Random a) => State g a
--randomSt = state random

--threeCoins :: State StdGen (Bool,Bool,Bool, Bool)
--threeCoins = do
--    a <- randomSt
--    b <- randomSt
--    c <- randomSt
--    d <- randomSt
--    return (a,b,c,d)

-- finalCountDownDiffList :: Int -> Writer (DiffList String) ()
-- finalCountDownDiffList 0 = do
--   tell (toDiffList ["0"])
-- finalCountDownDiffList x = do
--   finalCountDownDiffList (x-1)
--   tell (toDiffList [show x])


-- RPM Calculator

--rpn1 :: String -> Double
--rpn1 = head . foldl foldingFunction1 [] . words

--foldingFunction1 :: [Double] -> String -> [Double]
--foldingFunction1 (x:y:ys) "*" = (x * y):ys
--foldingFunction1 (x:y:ys) "+" = (x + y):ys
--foldingFunction1 (x:y:ys) "-" = (y - x):ys
--foldingFunction1 xs numberString = read numberString:xs

--foldingTest :: [Double] -> String -> [Double]
--foldingTest dList st = read st:dList

--rpn :: String -> Maybe Double
--rpn st = do
--  [res] <- foldM foldingFunction [] (words st)
--  return res

--readMaybe :: (Read a) => String -> Maybe a
--readMaybe st = case reads st of
--  [(x, "")] -> Just x
--  _         -> Nothing

--foldingFunction :: [Double] -> String -> Maybe [Double]
--foldingFunction (x:y:ys) "+" = return ((x + y ) : ys)
--foldingFunction (x:y:ys) "*" = return ((x * y ) : ys)
--foldingFunction (x:y:ys) "-" = return((x - y ) : ys)
--foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

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
 

  print "All Times Values" 
  print tsAll_1
  print ""
  print $ meanTS tsAll_1
  print $ minTS tsAll_1
  print $ maxTS tsAll_1
  print $ meanTS $ diffTS tsAll_1
  print $ movingAvarageTS tsAll_1 3

  putStrLn "END"

runRobotPart :: IO ()
runRobotPart  = do
  print $ renderHtml leftArm
  putStrLn "END"

students :: [Student]
students = [ (Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophomore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))]

teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simone" "De Beauvior")
           , Teacher 200 (Name "Susan" "Sontag")]

courses :: [Course]
courses = [ Course 101 "French" 100
          , Course 201 "English" 200]

__select :: [String]
__select = _select (lastName . studentName) students

__where :: [Name]
__where = _where (startWith 'J' . firstName) (_select studentName students)

__join :: [(Teacher, Course)]
__join = _join teachers courses teacherId teacher

jonData :: [(Teacher, Course)]
jonData = (_join teachers courses teacherId teacher)

whereResult :: [(Teacher, Course)]
whereResult = _where ((== "English") . courseTitle . snd) jonData

selectResult :: [Name]
selectResult = _select (teacherName . fst) whereResult