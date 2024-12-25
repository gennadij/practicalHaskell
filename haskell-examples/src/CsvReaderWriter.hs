-- {-# LANGUAGE OverloadedStrings #-}

module CsvReaderWriter(parseCsv) where

import Text.ParserCombinators.Parsec

{-
Es muss immer am ende der Datei ei \n sein.
-}

csvFile = endBy line eol
line = sepBy cell (char ';')
cell = quotedCell <|> many (noneOf ";\n\r")
quotedCell =
  do  char '"'
      content <- many quotedChar
      char '"' <?> "quote at end of cell"
      return content
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCsv :: String -> Either ParseError [[String]]
parseCsv = parse csvFile "(unknown)"





