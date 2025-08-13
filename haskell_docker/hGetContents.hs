import System.IO
import Data.List.Split

main = do
    withFile "test.csv" ReadMode (
        \handle -> do
            contents <- hGetContents handle
            let linesOfContents = lines contents
            --    zipLinesOfContents = zipWith(\n l -> show n ++ " " ++ l) [1..] linesOfContents
            --mapM putStr (extactLine (unlines linesOfContents))
            return ()
        )

extactLine :: String -> [String]
extactLine l = endBy ";" l

-- extractLinesFromFiles :: String -> [[String]]
-- extractLinesFromFiles filePath = do 
--     withFile filePath ReadMode (
--         \handle -> do
--            contents <- hGetContents handle
--            let linesOfContents = lines contents
--            return linesOfContents
--        )