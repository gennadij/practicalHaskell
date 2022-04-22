module Main where

import System.Environment
import Chapter9.FilesAndStrimes.FilesAndStrimes (readFileAndStreams)

main :: IO ()
main = do
  args <- getArgs
  readFileAndStreams (head args)
