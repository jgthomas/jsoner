module Validate (jsonParser) where

import Data.List (intercalate)
import Helper (lexeme, spaceParser)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import ValidateNumber (numberParser)
import ValidateString (stringParser)

jsonParser :: Parser String
jsonParser = do
  start <- lexeme $ M.string "{"
  contents <- keyValuesParser
  end <- lexeme $ M.string "}"
  pure $ start <> contents <> end

keyValuesParser :: Parser String
keyValuesParser = concat <$> M.sepEndBy keyValueParser (M.char ',')

keyValueParser :: Parser String
keyValueParser = do
  spaceParser
  key <- lexeme stringParser
  sep <- lexeme $ M.string ":"
  value <- valueParser
  pure $ key ++ sep ++ value

valueParser :: Parser String
valueParser = do
  spaceParser
  booleanValueParser
    <|> nullValueParser
    <|> objectValueParser
    <|> arrayValueParser
    <|> stringValueParser
    <|> numberValueParser
  where
    booleanValueParser = M.try $ lexeme (M.string "true" <|> M.string "false")
    nullValueParser = M.try $ lexeme $ M.string "null"
    objectValueParser = M.try $ lexeme $ jsonParser
    stringValueParser = M.try $ lexeme stringParser
    numberValueParser = M.try $ lexeme $ numberParser

arrayValueParser :: Parser String
arrayValueParser = M.try $ do
  start <- M.char '['
  contents <- M.sepEndBy valueParser (M.char ',')
  end <- M.char ']'
  pure $ [start] ++ intercalate "," contents ++ [end]
