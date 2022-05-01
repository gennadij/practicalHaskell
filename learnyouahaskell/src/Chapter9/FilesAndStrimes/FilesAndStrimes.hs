module Chapter9.FilesAndStrimes.FilesAndStrimes (
  readFileAndStreams, 
  onlyReadFile, 
  readAndWriteFile, 
  taskManager) where 

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Char (toUpper)

readFileAndStreams :: String -> IO ()
readFileAndStreams filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

onlyReadFile :: String -> IO ()
onlyReadFile filePath = do 
  content <- readFile filePath
  putStr content

readAndWriteFile :: String -> IO ()
readAndWriteFile filePath = do 
  content <- readFile filePath
  writeFile (filePath ++ ".changed")  (map toUpper content)
  contentChanged <- readFile (filePath ++ ".changed")
  putStr contentChanged

taskManager :: IO()
taskManager = do
  todoItem <- getLine 
  appendFile "todo.txt" (todoItem ++ "\n")