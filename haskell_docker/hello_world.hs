main = do
    putStrLn "Add name" 
    name <- getLine
    if null name
    then return ()
    else do 
        putStrLn $ "Name : " ++ name 
        main