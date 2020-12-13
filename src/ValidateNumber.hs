module ValidateNumber (numberValueParser) where

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Parser (Parser)

numberValueParser :: Parser String
numberValueParser = M.try $ M.some M.digitChar
