module Lib
    ( someFunc
    ) where
import GreaterThan ( initCars, initPeople )

someFunc :: IO ()
someFunc = do 
    let people = initPeople
    let cars = initCars
    print people
    print cars

