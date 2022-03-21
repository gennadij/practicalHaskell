module Chapter4.Nameable where

import Chapter3.Data (Client(..), Person(..)) 

class Nameable n where
  name :: n -> String 

instance Nameable (Client i) where
  name Individual {person = Person{fName = f, lName = n}} = f ++ " " ++ n
  name c = clientName c 
