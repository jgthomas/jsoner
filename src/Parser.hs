module Parser (Parser) where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String
