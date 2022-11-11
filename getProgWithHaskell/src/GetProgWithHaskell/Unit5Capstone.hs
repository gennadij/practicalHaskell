module GetProgWithHaskell.Unit5Capstone (
  _select, 
  Student(..), 
  GradeLevel(..), 
  Name(..)
) where

data Name = Name { firstName :: String
                 , lastName :: String }

instance Show Name where
  show (Name firstN lastN) = mconcat [firstN, " ", lastN]

data GradeLevel = Freshman
                 | Sophomore
                 | Junior
                 | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student { studentId :: Int
                       , gradeLevel :: GradeLevel
                       , studentName :: Name} deriving Show

_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
  val <- vals
  return (prop val)