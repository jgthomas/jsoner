module ValidateTestSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate an empty JSON" $
      M.parse jsonParser "" "{}"
        `shouldParse` "{}"
  describe "Valid JSON" $
    it "Should validate a JSON with single key-value pair" $
      M.parse jsonParser "" "{\"key\":\"value\"}"
        `shouldParse` "{\"key\":\"value\"}"
  describe "Invalid JSON" $
    it "Should fail validation with unquoted key" $
      M.parse jsonParser "{key:\"value\"}"
        `shouldFailOn` "k"
