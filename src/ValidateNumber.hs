module ValidateNumber (numberValueParser) where

import Parser (Parser, toStringParser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

numberValueParser :: Parser String
numberValueParser = M.try $ exponentParser <|> integerParser

integerParser :: Parser String
integerParser = M.try $ posIntegerParser <|> negIntegerParser

exponentParser :: Parser String
exponentParser = M.try $ do
  coeff <- integerParser
  e <- toStringParser $ M.char' 'e'
  sign <- signParser
  expon <- M.some M.digitChar
  pure $ coeff <> e <> sign <> expon

posIntegerParser :: Parser String
posIntegerParser = M.try $ M.some M.digitChar

negIntegerParser :: Parser String
negIntegerParser = M.try $ do
  neg <- negativeParser
  digits <- posIntegerParser
  pure $ neg <> digits

signParser :: Parser String
signParser = positiveParser <|> negativeParser

positiveParser :: Parser String
positiveParser = toStringParser $ M.char '+'

negativeParser :: Parser String
negativeParser = toStringParser $ M.char '-'
