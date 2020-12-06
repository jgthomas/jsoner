module Main (main) where

import System.Environment (getArgs)
import qualified Text.Megaparsec as M
import Validate (validJson)

main :: IO ()
main = do
  input <- fmap head getArgs
  M.parseTest validJson input
