module Chapter10.ReversePolish.ReversePolishCalculator where
{--
Der Modull berechnet Strings die reverse polish notation geschrieben.
--}
import Data.List

solveRPC :: String -> Char  
solveRPC expression = head (foldl calculate [] (words expression))
-- 	(a -> b -> a) -> a -> [b] -> a
calculate :: [Char] -> [Char] -> [Char]
calculate a b = []


