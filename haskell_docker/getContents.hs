-- getContents.hs
import System.IO

main = do 
    withFile "hello_world.hs" ReadMode (\handle -> do 
        contents <- hGetContents handle
        putStr contents)
