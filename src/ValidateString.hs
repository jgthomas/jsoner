module ValidateString (stringParser) where

import Data.Char (isAlphaNum)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

stringParser :: Parser String
stringParser = do
  openQuote <- M.char '"'
  content <- M.many innerCharParser
  closeQuote <- M.char '"'
  pure $ [openQuote] <> concat content <> [closeQuote]
  where
    innerCharParser = fmap pure (M.try unEscaped) <|> escaped

escaped :: Parser String
escaped = do
  d <- M.char '\\'
  c <- M.oneOf ['\\', '\"', '/']
  pure [d, c]

unEscaped :: Parser Char
unEscaped = M.noneOf ['\\', '\"', '/']
