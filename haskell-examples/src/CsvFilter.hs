module CsvFilter (getCollumsFiltered, getAllColumn, backTransformColumn) where

import Data.List (transpose)

getCollumsFiltered :: [[String]] -> [String] -> [[String]]
getCollumsFiltered lx = map (head . (\f -> filter (\l -> head l == f) lx ))

getAllColumn :: [[String]] -> Int -> [[String]]
getAllColumn l n = transpose (drop n l)

backTransformColumn :: [[String]] -> [[String]]
backTransformColumn = transpose

-- getCollumsFiltered :: [[String]] -> [String] -> [[String]]
-- getCollumsFiltered l f = filter (\x -> head x == head f) l

-- sizeOfDataRows :: [[String]] -> [Int]
-- sizeOfDataRows = map length

-- findHead :: [[String]] -> [[String]]
-- findHead = map (filter (=="Buchungsdatum"))

-- getRow :: [[String]] -> Int -> [String]
-- getRow l n = l !! n

-- dropFirstElems :: [[String]] -> Int -> [[String]]
-- dropFirstElems l n = drop n l