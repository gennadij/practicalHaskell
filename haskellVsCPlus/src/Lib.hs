module Lib
    ( someFunc
    ) where
import GreaterThan ( olderThan, initCars, initPeople, People(..), Car(..) )


someFunc :: IO ()
someFunc = do 
    let people = initPeople
    let cars = initCars

    let olderThan42 = olderThan 42
    let peoplesOlder42 = filter (\p -> olderThan42 (p_age p)) people
    let peoplesNameOlder42 = map p_name peoplesOlder42
    print (peoplesNameOlder42)

    let carsOlder42 = filter (\p -> olderThan42 (c_age p)) cars
    let carsNameOlder42 = map c_name carsOlder42
    print (carsNameOlder42)