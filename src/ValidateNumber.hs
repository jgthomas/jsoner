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
    negParser = negativeIntParser

integerParser :: Parser String
integerParser = M.try $ negInt <|> posInt
  where
    negInt = negativeIntParser
    posInt = M.some M.digitChar

negativeIntParser :: Parser String
negativeIntParser = do
  sign <- signParser
  digs <- M.some M.digitChar
  pure $ [sign] <> digs

signParser :: Parser Char
signParser = M.oneOf ['-', '+']
