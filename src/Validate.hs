module Validate (jsonParser) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = Parsec Void String

jsonParser :: Parser String
jsonParser = emptyBodyParser <|> fullBodyParser

emptyBodyParser :: Parser String
emptyBodyParser = M.try $ do
  start <- bodyStartParser
  end <- bodyEndParser
  pure [start, end]

fullBodyParser :: Parser String
fullBodyParser = M.try $ do
  start <- bodyStartParser
  contents <- keyValueParser
  end <- bodyEndParser
  pure $ [start] ++ contents ++ [end]

bodyStartParser :: Parser Char
bodyStartParser = M.char '{'

bodyEndParser :: Parser Char
bodyEndParser = M.char '}'

keyValueParser :: Parser String
keyValueParser = do
  key <- keyParser
  sep <- M.char ':'
  value <- valueParser
  pure $ key ++ [sep] ++ value

keyParser :: Parser String
keyParser = M.many M.alphaNumChar

valueParser :: Parser String
valueParser = M.many M.alphaNumChar
