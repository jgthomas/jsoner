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
    it "Should validate a JSON with single string key-value pair" $
      M.parse jsonParser "" "{\"key\":\"value\"}"
        `shouldParse` "{\"key\":\"value\"}"
  describe "Valid JSON" $
    it "Should validate a JSON with boolean TRUE value" $
      M.parse jsonParser "" "{\"key\":true}"
        `shouldParse` "{\"key\":true}"
  describe "Valid JSON" $
    it "Should validate a JSON with boolean FALSE value" $
      M.parse jsonParser "" "{\"key\":false}"
        `shouldParse` "{\"key\":false}"
  describe "Valid JSON" $
    it "Should validate a JSON with NULL value" $
      M.parse jsonParser "" "{\"key\":null}"
        `shouldParse` "{\"key\":null}"
  describe "Valid JSON" $
    it "Should validate a JSON with empty value" $
      M.parse jsonParser "" "{\"key\":\"\"}"
        `shouldParse` "{\"key\":\"\"}"
  describe "Invalid JSON" $
    it "Should fail validation with unquoted key" $
      M.parse jsonParser "{key:\"value\"}"
        `shouldFailOn` "k"
