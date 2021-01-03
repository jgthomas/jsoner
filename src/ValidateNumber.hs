module ValidateNumber (numberParser) where

import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

numberParser :: Parser String
numberParser = M.try $ exponentParser <|> integerParser

exponentParser :: Parser String
exponentParser = M.try $ do
  coeff <- coeffParser
  e <- M.char' 'e'
  sign <- signParser
  expon <- M.some M.digitChar
  pure $ coeff <> [e] <> [sign] <> expon

coeffParser :: Parser String
coeffParser = M.try $ negParser <|> posParser
  where
    posParser = M.some M.digitChar
    negParser = do
      neg <- M.char '-'
      digs <- M.some M.digitChar
      pure $ [neg] <> digs

integerParser :: Parser String
integerParser = M.try $ negativeIntParser <|> positiveIntegerParser
  where
    negativeIntParser = M.string "-0" <|> negNumberParser
    positiveIntegerParser = M.string "0" <|> posNumParser

posNumParser :: Parser String
posNumParser = do
  first <- positiveDigit
  rest <- M.many M.digitChar
  pure $ [first] <> rest

negNumberParser :: Parser String
negNumberParser = do
  neg <- M.char '-'
  num <- posNumParser
  pure $ [neg] <> num

positiveDigit :: Parser Char
positiveDigit = M.oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9']

signParser :: Parser Char
signParser = M.oneOf ['-', '+']
