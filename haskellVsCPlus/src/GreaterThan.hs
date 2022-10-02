module GreaterThan (
    People (..), 
    Car (..), 
    initCars, 
    initPeople,
    olderThan
) where

data People = P {
    p_name :: String,
    p_gender :: String,
    p_age :: Int
} deriving Show

data Car = C {
    c_name :: String,
    c_typ :: String,
    c_age :: Int
} deriving Show

initCars :: [Car]
initCars = [
    C "BMW" "pkw" 10, 
    C "Audi" "pkw" 20,
    C "Kamaz" "lkw" 30,
    C "Lada" "pkw" 40,
    C "Honda" "pkw" 50,
    C "Merceres" "pkw" 60,
    C "Mercedes" "lkw" 70,
    C "vw" "pkw" 80,
    C "Honda" "pkw" 90
  ]

initPeople :: [People]
initPeople = [
    P "Marta" "female" 10, 
    P"Peter" "male" 20,
    P "Sandra" "female" 30,
    P "Lucy" "female" 40,
    P "Mark" "male" 50,
    P "Anton" "male" 60,
    P "Luda" "female" 70,
    P "Artiom" "male" 80,
    P "Aljona" "female" 90
  ]

olderThan :: Int -> Int -> Bool
olderThan a b = a > b