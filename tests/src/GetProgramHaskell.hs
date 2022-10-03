module GetProgramHaskell () where

-- Lesson 5
getRequestUrl host apiKey resource id = 
    host ++ "/" ++ resource ++ "/" ++ id ++ "/" ++ apiKey

getHostRequestBuilder host = 
    (\apiKey resource id -> getRequestUrl host apiKey resource id)

exampleHostBuilder = getHostRequestBuilder "http://test.de"


inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n*2

square :: Num a => a -> a
square n = n^2

ifEven :: Integral t => (t -> t) -> t -> t
ifEven f x = if even x
             then f x
             else x

ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven inc 

ifEvenDouble :: Integer -> Integer
ifEvenDouble = ifEven double 

ifEvenSquare :: Integer -> Integer
ifEvenSquare = ifEven square

-- Lesson 8

myTake :: (Num n, Eq n) => n -> [a] -> [a]
myTake _ [] = []
myTake 0 _  = []
myTake n (x:xs) = x:rest
  where rest = myTake (n - 1) xs

--Lesson 10

cup :: Int -> (Int -> Int) -> Int
cup mililiter = \m -> m mililiter

fMililiter :: Int -> Int
fMililiter n = n

getMililiter :: ((Int -> Int) -> Int) -> Int
getMililiter aCup = aCup fMililiter

drink :: ((Int -> Int) -> Int) -> Int -> ((Int -> Int) -> Int)
drink aCup mlDrank = cup (mililiter - mlDrank)
  where mililiter = getMililiter aCup

type TRobot = ([Char], Int, Int)

robot :: TRobot -> (TRobot -> t) -> t 
robot (name, attack, hp) = \f -> f (name, attack, hp)

name :: TRobot -> [Char]
name (n, _, _) = n

attack :: TRobot -> Int
attack (_, a, _) = a

hp :: TRobot -> Int
hp (_, _, hp) = hp

getName :: ((TRobot -> [Char]) -> [Char]) -> [Char]
getName aRobot = aRobot name

getAttack :: ((TRobot -> Int) -> Int) -> Int
getAttack aRobot = aRobot attack

getHP :: ((TRobot -> Int) -> Int) -> Int
getHP aRobot = aRobot hp