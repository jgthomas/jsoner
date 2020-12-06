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
  M.char '{'
  M.char '}'
  pure "{}"
