module Validate (validJson) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = Parsec Void String

validJson :: Parser String
validJson = emptyJsonBody

emptyJsonBody :: Parser String
emptyJsonBody = do
  start <- bodyStart
  end <- bodyEnd
  pure [start, end]

bodyStart :: Parser Char
bodyStart = M.char '{'

bodyEnd :: Parser Char
bodyEnd = M.char '}'
