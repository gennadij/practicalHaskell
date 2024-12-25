module Lib() where

-- import Text.ParserCombinators.Parsec
--     ( char,
--       noneOf,
--       eof,
--       (<|>),
--       many,
--       parse,
--       ParseError,
--       GenParser,
--       Parser )

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
-- import Text.Parsec.String.Parsec (try)
-- import Text.Parsec.String.Char (oneOf, char, digit, string, letter, satisfy)
-- import Text.Parsec.String.Combinator (many1, choice, chainl1, between
--                                     ,count, option, optionMaybe, optional)
import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
import Control.Monad (void, ap, mzero)
import Data.Char (isLetter, isDigit)
-- import FunctionsAndTypesForParsing

-- csvFile :: GenParser Char st [[String]]
-- csvFile = do
--   result <- many line
--   eof
--   return result

-- line :: GenParser Char st [String]
-- line = do
--   result <- cells
--   eof
--   return result

-- cells :: GenParser Char st [String]
-- cells = do
--   first <- cellContent
--   next <- remainingCells
--   return (first : next)

-- remainingCells :: GenParser Char st [String]
-- remainingCells = (char ',' >> cells) <|> return []

-- cellContent :: GenParser Char st String
-- cellContent = many (noneOf ",\n")

-- eol :: GenParser Char st Char
-- eol = char '\n'

-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV = parse csvFile "(unknown)"

