module ValidateNumber (numberValueParser) where

import Parser (Parser, toStringParser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

numberValueParser :: Parser String
numberValueParser = M.try $ integerParser

integerParser :: Parser String
integerParser = M.try $ posIntegerParser <|> negIntegerParser
  where
    posIntegerParser :: Parser String
    posIntegerParser = M.some M.digitChar

    negIntegerParser :: Parser String
    negIntegerParser = do
      neg <- negativeParser
      digits <- posIntegerParser
      pure $ neg <> digits

signParser :: Parser String
signParser = positiveParser <|> negativeParser

positiveParser :: Parser String
positiveParser = toStringParser $ M.char '+'

negativeParser :: Parser String
negativeParser = toStringParser $ M.char '-'
