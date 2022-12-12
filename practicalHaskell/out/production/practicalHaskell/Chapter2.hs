{-#LANGUAGE ViewPatterns#-}
-- {-#LANGUAGE NamedFieldPuns#-}
-- {-#LANGUAGE RecordWildCards#-}

module Chapter2 where

import OwnData

testFoldr :: IO ()
testFoldr = print $ foldr (-) 0 [1,2,3,4,5]

testFoldl :: IO ()
testFoldl = print $ foldl (-) 0 [1,2,3,4,5]

infMax :: Ord a => InfNumber a -> InfNumber a -> InfNumber a
infMax MinusInf x = x
infMax x MinusInf = x
infMax PlusInf _ = PlusInf
infMax _ PlusInf = PlusInf
infMax (Number a) (Number b) = Number (max a b)

testInfFoldr :: IO ()
testInfFoldr = print $ foldr infMax MinusInf $ map Number [1,2,3,4,5]

specialClient :: Client -> Bool
specialClient (clientName -> "Mustername") = True
specialClient (companyId -> 123)       = True
specialClient _                            = False

greet :: Client -> String
greet :: Individual { person = Person { fName = fN } } = "Hallo Individual " ++ fN
greet :: Company { clientName = c } = "Hallo Company " ++ c
greet :: GovOrg { } = "Hallo GovOrg"




