module ValidateNumber (numberParser) where

import Helper (lexeme)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

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
    posInt = show <$> integer

negativeIntParser :: Parser String
negativeIntParser = do
  neg <- M.char '-'
  digs <- show <$> integer
  pure $ [neg] <> digs

signParser :: Parser Char
signParser = M.oneOf ['-', '+']

integer :: Parser Integer
integer = lexeme L.decimal
