module ValidateString (stringParser) where

import Data.Char (toLower)
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
    innerCharParser =
      fmap pure (M.try unEscaped)
        <|> (M.try hexParser)
        <|> escaped

unEscaped :: Parser Char
unEscaped = M.noneOf ['\\', '\"', '/']

escaped :: Parser String
escaped = do
  d <- M.char '\\'
  c <- M.oneOf ['\\', '\"', '/']
  pure [d, c]

hexParser :: Parser String
hexParser = do
  escape <- M.string "\\u"
  n0 <- hexDigit
  n1 <- hexDigit
  n2 <- hexDigit
  n3 <- hexDigit
  pure $ escape <> [n0, n1, n2, n3]

hexDigit :: Parser Char
hexDigit = (hexLetterDigit <|> M.digitChar)

hexLetterDigit :: Parser Char
hexLetterDigit = M.satisfy isHexLetterDigit

isHexLetterDigit :: Char -> Bool
isHexLetterDigit c = toLower c `elem` ['a', 'b', 'c', 'd', 'e', 'f']
