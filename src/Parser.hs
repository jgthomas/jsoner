module Parser (Parser, toStringParser) where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

toStringParser :: Parser Char -> Parser String
toStringParser parChar = (: []) <$> parChar
