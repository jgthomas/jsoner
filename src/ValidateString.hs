module ValidateString (stringParser) where

import Parser (Parser)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

stringParser :: Parser String
stringParser = do
  openQuote <- M.char '"'
  content <- M.many M.alphaNumChar
  closeQuote <- M.char '"'
  pure $ [openQuote] <> content <> [closeQuote]
