import System.IO
import Data.List.Split

main = do
    contents <- readFile "test.csv"  
    let linesOfContents = lines contents
            --    zipLinesOfContents = zipWith(\n l -> show n ++ " " ++ l) [1..] linesOfContents
    mapM putStrLn (linesOfContents)

getTestString :: [String]
getTestString = ["test1;test2;test3;","test4;test5;test6;"]

extactLine :: String -> [String]
extactLine l = endBy ";" l

extractLines :: [[String]]
extractLines = map extactLine getTestString 

extractLinesFromFile :: String -> IO [[String]]
extractLinesFromFile fileName = do
    contents <- readFile fileName
    let linesOfContents = lines contents
        extractedLines = map extactLine linesOfContents
        removeAtPositions = dropPCollumsAtPositions [0, 2] extractedLines
    return removeAtPositions

dropPCollumsAtPositions :: [Int] ->  [[String]] -> [[String]]
dropPCollumsAtPositions positions rows = map (removePositions positions) rows
    where
        removePositions pos row = [x | (i, x) <- zip [0..] row, not (i `elem` pos)]

-- collectCSV :: [[String]]-> [String]
-- collectCSV :: 