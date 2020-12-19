module Helper (spaceParser, nullValueParser) where

import Control.Applicative (empty)
import Parser (Parser)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

-- | Consumes whitespace, with no line or block comments defined
spaceParser :: Parser ()
spaceParser = L.space M.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceParser

-- | Parse the string null followed by any whitespace
nullValueParser :: Parser String
nullValueParser = lexeme (M.string "null")
