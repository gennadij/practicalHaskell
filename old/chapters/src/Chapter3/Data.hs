module Chapter3.Data (Client (..), Person(..)) where

data Client i = GovOrg     { clientId :: i, clientName :: String }
              | Company    { clientId :: i, clientName :: String
                           , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

data Person = Person { fName :: String, lName :: String } 
             deriving (Show , Eq, Ord)

