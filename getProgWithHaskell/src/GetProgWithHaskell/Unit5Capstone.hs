module GetProgWithHaskell.Unit5Capstone (
  _select,
  _where,
  startWith,
  Student(..),
  GradeLevel(..),
  Name(..)
) where

import Control.Monad

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

_where :: (a -> Bool) -> [a] -> [a]
_where test vals = do
  val <- vals
  guard (test val)
  return val

startWith :: Char -> String -> Bool
startWith char string = char == (head string)

