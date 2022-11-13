module GetProgWithHaskell.Unit5Capstone (
  _select,
  _where,
  startWith,
  Student(..),
  GradeLevel(..),
  Name(..),
  Teacher( .. ),
  Course ( .. ),
  _join
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

data Teacher = Teacher { teacherId :: Int
                       , teacherName :: Name} deriving Show

data Course = Course { courseId :: Int
                     , courseTitle :: String
                     , teacher :: Int} deriving Show

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

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dataPairs = (d1, d2)
  guard ((prop1 (fst dataPairs)) == (prop2 (snd dataPairs)))
  return dataPairs

