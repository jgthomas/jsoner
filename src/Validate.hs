module Validate (jsonParser) where

import Data.List (intercalate)
import Helper (lexeme)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import ValidateNumber (numberParser)
import ValidateString (stringParser)

-- | Validates JSON
jsonParser :: Parser String
jsonParser = do
  start <- lexeme $ M.string "{"
  contents <- M.sepEndBy keyValueParser (lexeme $ M.char ',')
  end <- lexeme $ M.string "}"
  pure $ start <> intercalate "," contents <> end

keyValueParser :: Parser String
keyValueParser = do
  key <- lexeme stringParser
  sep <- lexeme $ M.string ":"
  value <- valueParser
  pure $ key <> sep <> value

valueParser :: Parser String
valueParser =
  M.try $
    booleanValueParser
      <|> nullValueParser
      <|> objectValueParser
      <|> arrayValueParser
      <|> numberValueParser
      <|> stringValueParser
  where
    booleanValueParser = lexeme (M.string "true" <|> M.string "false")
    nullValueParser = lexeme $ M.string "null"
    objectValueParser = lexeme jsonParser
    arrayValueParser = lexeme arrayParser
    numberValueParser = lexeme numberParser
    stringValueParser = lexeme stringParser

arrayParser :: Parser String
arrayParser = do
  start <- M.string "["
  contents <- M.sepEndBy valueParser (lexeme $ M.char ',')
  end <- M.string "]"
  pure $ start <> intercalate "," contents <> end
