module ValidateHex (hexParser) where

import Data.Char (toLower)
import Parser (Parser)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

hexParser :: Parser String
hexParser = do
  startQuote <- M.char '\"'
  escape <- M.string "\\u"
  number <- M.some (hexLetterDigit <|> M.digitChar)
  endQuote <- M.char '\"'
  pure $ [startQuote] <> escape <> number <> [endQuote]

hexLetterDigit :: Parser Char
hexLetterDigit = M.satisfy isHexLetterDigit

isHexLetterDigit :: Char -> Bool
isHexLetterDigit c = toLower c `elem` ['a', 'b', 'c', 'd', 'e', 'f']
