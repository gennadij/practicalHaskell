module Main where

import Lib
import Chapter2.Section2.Example (exampleFunc)
import Data.Type.Bool (Not)

main :: IO ()
main = do 
  someFunc
  exampleFunc
  monadExample
  putStrTwice


monadExample :: IO ()
monadExample = do 
  putStrLn $ testMaybe returnMonad 
  putStrLn bindMonad

returnMonad :: Maybe String
returnMonad = return "test"

bindMonad :: String 
bindMonad = testMaybe (Just "test2" >>= bindTest)

bindTest :: String -> Maybe String
bindTest str = Just (reverse str)

testMaybe :: Maybe String -> String
testMaybe (Just a) = a
testMaybe Nothing = ""

putStrTwice :: IO ()
putStrTwice = putStrLn "test" >> putStrLn "test2"

-- ============================================

data Person = Person {
  fName :: String,
  lName :: String
} deriving Show

personFromTuple :: (String, String) -> Person
personFromTuple (fName, lName) = Person fName lName

convertTupleFunctor :: Functor f => f (String, String) -> f Person
convertTupleFunctor = fmap personFromTuple