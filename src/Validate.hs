module Validate (jsonParser) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = Parsec Void String

jsonParser :: Parser String
jsonParser = do
  start <- startParser
  contents <- contentsParser
  end <- endParser
  pure $ [start] ++ contents ++ [end]
  where
    startParser = M.char '{'
    contentsParser = keyValueParser
    endParser = M.char '}'

keyValueParser :: Parser String
keyValueParser = do
  key <- keyParser
  sep <- M.char ':'
  value <- valueParser
  pure $ key ++ [sep] ++ value

keyParser :: Parser String
keyParser = M.many M.alphaNumChar

valueParser :: Parser String
valueParser = bodyValueParser <|> arrayValueParser <|> stringValueParser
  where
    stringValueParser :: Parser String
    stringValueParser = M.try (M.many M.alphaNumChar)

    bodyValueParser :: Parser String
    bodyValueParser = M.try jsonParser

    arrayValueParser :: Parser String
    arrayValueParser = M.try $ do
      start <- M.char '['
      contents <- M.many M.alphaNumChar
      end <- M.char ']'
      pure $ [start] ++ contents ++ [end]
