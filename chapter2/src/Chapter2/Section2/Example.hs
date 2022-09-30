{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns#-}
module Chapter2.Section2.Example
(exampleFunc) where

import  DataTypes.DataTypes  (Client (..), Person (..), ClientR (..), PersonR (..))

exampleFunc :: IO ()
exampleFunc = do
  putStrLn (getClientName clientGovOrg)
  putStrLn (specialClient clientGovOrg)
  putStrLn (greetPuns clientGovOrgR)

-- Data
clientGovOrg :: Client
clientGovOrg = GovOrg "Herr Heimann"

clientGovOrgR :: ClientR 
clientGovOrgR = GovOrgR "Herr Mueller"

-- Logic
getClientName :: Client -> String 
getClientName c = case c of
  GovOrg name -> name
  Company name _ _ _ -> name
  Individual (Person fName lName _ ) _ -> fName ++ " " ++ lName

specialClient :: Client -> String
specialClient (getClientName -> "Herr Heimann") = "0"
specialClient (responsibility -> "Boos")           = "1"
specialClient _                              = "2"

responsibility :: Client -> String 
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknow"

greet :: ClientR -> String 
greet IndividualR {personR = PersonR {fNameR = fn}} = "Hi " ++ fn
greet CompanyR { clientRName = c}                   = "Hei " ++ c
greet GovOrgR { }                                   = "Wilcome"

greetPuns :: ClientR -> String 
greetPuns IndividualR {personR = PersonR { fNameR }} = "Hi " ++ fNameR
greetPuns CompanyR { clientRName }                   = "Hei " ++ clientRName
greetPuns GovOrgR { }                                = "Wilcome"
