module Main where

import System.Environment
import Chapter9.FilesAndStrimes.FilesAndStrimes (
  readFileAndStreams, onlyReadFile, readAndWriteFile, taskManager)

-- Param: ./src/Chapter9/FilesAndStrimes/girlfriend.txt
main :: IO ()
main = do
  args <- getArgs
  -- print "=== ReadFile with Handler ==="
  -- readFileAndStreams (head args)
  -- print "=== ReadFile without Handler === "
  -- onlyReadFile (head args)
  -- print "=== Reade And Write File ==="
  -- readAndWriteFile (head args)
  taskManager
