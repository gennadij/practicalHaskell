--{-#LANGUAGE NamedFieldPuns#-}

module Chapter3 where

import qualified Data.List as L
import OwnData

--duplicateOdds = map(*2) . filter odd

permutationsStartWith :: Char -> String -> [String]
permutationsStartWith letter = filter (\l -> head l == letter) . L.permutations

ownPartition = L.partition (> 0) [1,3,5,6,-8,-5,-3,9]

ownDropWhile = L.dropWhile (/= "test3") ["test1", "test2", "test3", "test4"]

ownTakeWhile = L.takeWhile (/= "test3") ["test1", "test2", "test3", "test4"]

ownSpan = L.span (/= "test3") ["test1", "test2", "test3", "test4"]

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual {person = p1}) (Individual{person = p2}) = compare (fName p1) (fName p2)
compareClient (Individual {}) _            = GT
compareClient _               (Individual {}) = LT
compareClient c1              c2           = compare (clientName c1) (clientName c2)

listOfClients = [ Individual 2 (Person "H. G." "Wells" Male)
                , GovOrg 3 "NTTF"  -- National Time Travel Foundation
                , GovOrg 6 "GTTF"  -- National Time Travel Foundation
                , Company 4 "NOrmhole Inc." 1234 (Person "Karl" "Schwarzschild" Male) "Physicist"
                , Individual 5 (Person "Doctor" "" Female)
                , Individual 6 (Person "Sarah" "Jane" Female)]

ownSortBy = L.sortBy compareClient listOfClients