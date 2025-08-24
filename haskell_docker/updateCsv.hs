import System.IO
import Data.List.Split
import System.Environment (getArgs)

-- updateCsv -f test.csv -o updated.csv --to-delete -c 1,4,5 -r 1,5,6
type InFile = String
type OutFile = String
type ColumnsToDelete = [Int]
type RowsToDelete = [Int]

main = do
    args <- getArgs
    -- TODO (gh) check if missing parameters
    let arg_f = filter (== "-f") args
        arg_fileName = if null arg_f then "" else args !! 1
        arg_o = filter (== "-o") args
        arg_outputFileName = if null arg_o then "" else args !! 3
        arg_to_delete = filter (== "--to-delete") args
        arg_c = filter (== "-c") args
        args_columnsToDelete = if null arg_c then [] else splitOn "," (args !! 6)
        arg_r = filter (== "-r") args
        args_rowsToDelete = if null arg_r then [] else splitOn "," (args !! 8)
    -- putStrLn $ head arg_f
    -- putStrLn arg_fileName
    -- putStrLn $ head  arg_o
    -- putStrLn $ arg_outputFileName
    -- putStrLn $ head arg_to_delete
    -- putStrLn $ head arg_c
    -- mapM putStrLn $ args_columnsToDelete
    -- putStrLn $ head arg_r
    -- mapM putStrLn $ args_rowsToDelete
    updateCsv arg_fileName arg_outputFileName (convertStringToIntList  args_columnsToDelete) (convertStringToIntList args_rowsToDelete)
        where convertStringToIntList xs = map read xs :: [Int]

updateCsv :: InFile -> OutFile -> ColumnsToDelete -> RowsToDelete ->  IO ()
updateCsv inFile outFile columnsToDelete rowsToDelete = do
    contents <- readFile inFile
    let linesOfContents = lines contents
        extractedLines = map extactLine linesOfContents
        removedRowsAtPositions = dropRowsAtPositions rowsToDelete extractedLines
        removedColumsAtPositions = dropCollumsAtPositions columnsToDelete removedRowsAtPositions
        reveresedColumns = reverseCollumns removedColumsAtPositions
        collectedNewCSV = collectCSV reveresedColumns
    writeFile outFile collectedNewCSV

dropCollumsAtPositions :: [Int] ->  [[String]] -> [[String]]
dropCollumsAtPositions positions rows = map (removePositions positions) rows
    where
        removePositions pos row = [x | (i, x) <- zip [0..] row, not (i `elem` pos)]

dropRowsAtPositions :: [Int] -> [[String]] -> [[String]]
dropRowsAtPositions positions rows = [x | (i, x) <- zip [0..] rows, not (i `elem` positions)]

collectCSV :: [[String]] -> String 
collectCSV xs = addNewLine ( map addSimicolon xs )
    where
        addSimicolon xs = concatMap (++ ";") xs
        addNewLine xs = concatMap (++ "\n") xs

extactLine :: String -> [String]
extactLine l = endBy ";" l

reverseCollumns :: [[String]] -> [[String]]
reverseCollumns = reverse

----------------------------------------------------------------------------
extractLinesFromFile :: String -> IO ()
extractLinesFromFile fileName = do
    contents <- readFile fileName
    let linesOfContents = lines contents
        extractedLines = map extactLine linesOfContents
        removeAtPositions = dropCollumsAtPositions [1] extractedLines
        collectedNewCSV = collectCSV removeAtPositions
    writeFile "updated.csv" collectedNewCSV

getTestString :: [String]
getTestString = ["test1;test2;test3;","test4;test5;test6;"]

extractLines :: [[String]]
extractLines = map extactLine getTestString 
        