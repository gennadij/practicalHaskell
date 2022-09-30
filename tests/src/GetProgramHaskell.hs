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