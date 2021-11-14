module DataTypes.DataTypes where
import Control.Monad.Identity (Identity)
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