module Helper (spaceParser, lexeme) where

import Control.Applicative (empty)
import Parser (Parser)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

-- | Consumes whitespace, with no line or block comments defined
spaceParser :: Parser ()
spaceParser = L.space M.space1 empty empty

-- | Create parser for element plus trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceParser

