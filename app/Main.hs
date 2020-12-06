module Main (main) where

import System.Environment (getArgs)
import qualified Text.Megaparsec as M
import Validate (jsonParser)

main :: IO ()
main = do
  input <- fmap head getArgs
  M.parseTest jsonParser input
