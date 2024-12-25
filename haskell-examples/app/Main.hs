module Main (main) where

import CsvReaderWriter (parseCsv, getAllColumn)

import CsvFilter(getCollumsFiltered, getAllColumn)

import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import System.Environment(getArgs)

data Config = Config {
  firstRowOfData :: Int,
  headerToRead :: [String]
} deriving (Show)

initConfigData :: Config
initConfigData = Config 4 ["Wertstellung", "Zahlungspflichtige*r", "Verwendungszweck"]


main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    case parseCsv contents of
        Left x -> print x
        Right results -> print $ getCollumsFiltered getAllColumns getHeadersToRead
    hClose handle
    where
      getAllCollumns = getAllColumn results (firstRowOfData initConfigData)
      getHeadersToRead = headerToRead initConfigData