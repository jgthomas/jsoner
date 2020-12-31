module Validate (jsonParser) where

import Data.List (intercalate)
import Helper (lexeme, spaceParser)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import ValidateNumber (numberValueParser)

jsonParser :: Parser String
jsonParser = emptyBodyParser <|> fullBodyParser

emptyBodyParser :: Parser String
emptyBodyParser = M.try $ do
  start <- bodyStartParser
  spaceParser
  end <- bodyEndParser
  pure [start, end]

fullBodyParser :: Parser String
fullBodyParser = M.try $ do
  start <- bodyStartParser
  contents <- keyValuesParser
  end <- bodyEndParser
  pure $ [start] ++ contents ++ [end]

bodyStartParser :: Parser Char
bodyStartParser = M.char '{'

bodyEndParser :: Parser Char
bodyEndParser = M.char '}'

keyValuesParser :: Parser String
keyValuesParser = concat <$> M.sepEndBy keyValueParser (M.char ',')

keyValueParser :: Parser String
keyValueParser = do
  spaceParser
  key <- keyParser
  spaceParser
  sep <- separatorParser
  value <- valueParser
  pure $ key ++ sep ++ value

keyParser :: Parser String
keyParser = do
  openQuote <- M.char '"'
  key <- M.some M.alphaNumChar
  closeQuote <- M.char '"'
  pure $ [openQuote] <> key <> [closeQuote]

valueParser :: Parser String
valueParser = do
  spaceParser
  booleanValueParser
    <|> nullValueParser
    <|> objectValueParser
    <|> arrayValueParser
    <|> stringValueParser
    <|> numberValueParser

-- | Parse the string null followed by any whitespace
nullValueParser :: Parser String
nullValueParser = lexeme (M.string "null")

-- | Parse the string 'true' or 'false' followed by any whitespace
booleanValueParser :: Parser String
booleanValueParser = M.try (trueParser <|> falseParser)
  where
    trueParser = lexeme (M.string "true")
    falseParser = lexeme (M.string "false")

separatorParser :: Parser String
separatorParser = lexeme (M.string ":")

stringValueParser :: Parser String
stringValueParser = M.try $ do
  openQuote <- M.char '"'
  content <- M.many M.alphaNumChar
  closeQuote <- M.char '"'
  pure $ [openQuote] <> content <> [closeQuote]

objectValueParser :: Parser String
objectValueParser = M.try jsonParser

arrayValueParser :: Parser String
arrayValueParser = M.try $ do
  start <- M.char '['
  contents <- M.sepEndBy valueParser (M.char ',')
  end <- M.char ']'
  pure $ [start] ++ intercalate "," contents ++ [end]
