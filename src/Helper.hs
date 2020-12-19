module Helper (sc) where

import Control.Applicative (empty)
import Parser (Parser)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space M.space1 empty empty
