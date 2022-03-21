{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Vector v where
  distance :: v -> v -> Double 

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)



