module Validate (jsonParser) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = Parsec Void String

jsonParser :: Parser String
jsonParser = emptyBodyParser

emptyBodyParser :: Parser String
emptyBodyParser = do
  start <- bodyStartParser
  end <- bodyEndParser
  pure [start, end]

bodyStartParser :: Parser Char
bodyStartParser = M.char '{'

bodyEndParser :: Parser Char
bodyEndParser = M.char '}'
