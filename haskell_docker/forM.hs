import Control.Monad

main = do
    colors <- forM[1,2,3](\a -> do
        putStrLn $ "Color: " ++ show a 
        color <- getLine
        return color)
    putStrLn "Colors"
    mapM putStrLn colors
