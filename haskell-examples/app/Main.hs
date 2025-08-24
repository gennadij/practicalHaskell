module Main (main) where
import Data.Char (isDigit)

-- import CsvReaderWriter (parseCsv)

-- import CsvFilter(getCollumsFiltered, getAllColumn, backTransformColumn)

-- import System.IO
--     ( IOMode(ReadMode, WriteMode), hClose, hGetContents, openFile, hPrint, hPutStr )
-- import System.Environment(getArgs)
-- import Data.List (intercalate)
-- import Text.Read (Lexeme(String))

-- data Config = Config {
--   firstRowOfData :: Int,
--   headerToRead :: [String],
--   outPutPath :: String
-- } deriving (Show)

-- initConfigData :: Config
-- initConfigData = Config 4 ["Wertstellung", "Betrag (€)"] "src/resource/test.csv"

-- main :: IO ()
-- main = do
--     args <- getArgs
--     handleIn <- openFile (head args) ReadMode
--     handleOut <- openFile (outPutPath initConfigData) WriteMode
--     contents <- hGetContents handleIn
--     case parseCsv contents of
--         Left x -> print x
--         Right results -> hPutStr handleOut (mapThsiList (getSelectedColumns results))
--           -- print $ getSelectedColumns results
--     hClose handleIn
--     hClose handleOut
--     where
--       getAllColumns r = getAllColumn r (firstRowOfData initConfigData)
--       getHeadersToRead = headerToRead initConfigData
--       getSelectedColumns :: [[String]] -> [[String]]
--       getSelectedColumns r = backTransformColumn (getCollumsFiltered (getAllColumns r) getHeadersToRead)
--       foldList :: [String] -> String
--       foldList = foldl (\x y -> x ++ y ++ "; ") ""
--       mapThsiList :: [[String]] -> String
--       mapThsiList xs = foldList (concat xs)


-- [["test1", "test2"], ["test3", "test4"]]

main :: IO ()
main = do
  putStrLn "--- Programm hat gestartet ---"
  putStrLn "Für Berechnung der Summen <s> eingeben"
  putStrLn "Für Beenden des Programms <q> eingeben"
  command <- getLine
  case command of 
    "s" -> do
      putStrLn "Geben Sie erste Zahl ein : "
      line1 <- getLine
      if isDigets line1 then do
        putStrLn "Geben Sie zweite Zahl ein : "
        line2 <- getLine
        if isDigets line2 then do
          putStrLn $ "Ergebnis : " ++ sum' line1 line2
          main
        else do
          putStrLn "Es wurde kein Zahl eingegeben !!!Abbrechen!!!"
          main  
      else do 
        putStrLn "Es wurde kein Zahl eingegeben !!!Abbrechen!!!"
        main      
    "q" -> return ()
    _ -> do 
      putStrLn "Unbekannte Eingabe"
      main

sum' :: String -> String -> String
sum' a b = 
  show ((read a :: Integer) + (read b :: Integer)) 

isDigets :: String -> Bool
isDigets = all isDigit

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) = 
  | abs ((left + n - right)) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = 
  | (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                = Nothing

x -: f = f x
