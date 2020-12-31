module ValidateStringSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import ValidateString (stringParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a JSON with a string value" $
      M.parse stringParser "" "\"value\""
        `shouldParse` "\"value\""
