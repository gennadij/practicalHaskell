module Chapter9.FilesAndStrimes.FilesAndStrimes (readFileAndStreams) where 

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

readFileAndStreams :: String -> IO ()
readFileAndStreams filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle