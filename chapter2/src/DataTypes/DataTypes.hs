module DataTypes.DataTypes (Client (..), Person (..), ClientR (..), PersonR (..)) where

type ClientName = String 
type IdNumber = Integer
type Position = String

data Client = GovOrg ClientName  
            | Company ClientName IdNumber Person Position 
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender deriving Show

data Gender = Male | Female | Unknowe deriving Show

type Manufacturer = String 
type Model = Integer
type TMName = String
type Price = Double  

data TimeMachine = TimeMachine Manufacturer Model TMName Price deriving Show

data ClientR = GovOrgR     {   clientRName :: String     }
             | CompanyR    {   clientRName :: String
                             , companyRId  :: Integer
                             , personR     :: PersonR
                             , positionR   :: String     }
             | IndividualR {   personR      :: PersonR    } deriving Show

data PersonR = PersonR     {   fNameR       :: String
                             , lNameR       :: String     } deriving Show

class Nameable n where
  name :: n -> String 

instance Nameable (ClientR i) where 
  name IndividualR {personR = PersonR{fNameR = f, lNameR = n}} = f ++ " " ++ name
  name c = clientRName c