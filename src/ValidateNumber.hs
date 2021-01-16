module ValidateNumber (numberParser) where

import Data.Char (toLower)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

-- | Parse a number value
numberParser :: Parser String
numberParser = M.try $ exponentParser <|> integerParser <|> hexParser

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

hexParser :: Parser String
hexParser = do
  startQuote <- M.char '\"'
  escape <- M.string "\\u"
  number <- M.some (hexLetterDigit <|> M.digitChar)
  endQuote <- M.char '\"'
  pure $ [startQuote] <> escape <> number <> [endQuote]

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

hexLetterDigit :: Parser Char
hexLetterDigit = M.satisfy isHexLetterDigit


signParser :: Parser Char
signParser = M.oneOf ['-', '+']

isHexLetterDigit :: Char -> Bool
isHexLetterDigit c = toLower c `elem` ['a', 'b', 'c', 'd', 'e', 'f']
