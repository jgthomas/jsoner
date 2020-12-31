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
    it "Should validate an empty JSON internal whitespace" $
      M.parse jsonParser "" "{   }"
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
  describe "Valid JSON" $
    it "Should validate a JSON with number value" $
      M.parse jsonParser "" "{\"key\":12345}"
        `shouldParse` "{\"key\":12345}"
  describe "Valid JSON" $
    it "Should validate a JSON with ZERO number value" $
      M.parse jsonParser "" "{\"key\":0}"
        `shouldParse` "{\"key\":0}"
  describe "Valid JSON" $
    it "Should validate a JSON with NEGATIVE number value" $
      M.parse jsonParser "" "{\"key\":-1002}"
        `shouldParse` "{\"key\":-1002}"
  describe "Valid JSON" $
    it "Should validate a JSON key with leading space" $
      M.parse jsonParser "" "{   \"key\":-1002}"
        `shouldParse` "{\"key\":-1002}"
  describe "Valid JSON" $
    it "Should validate a JSON key with trailing space" $
      M.parse jsonParser "" "{\"key\"  :-1002}"
        `shouldParse` "{\"key\":-1002}"
  describe "Valid JSON" $
    it "Should validate a JSON key with leading and trailing space" $
      M.parse jsonParser "" "{   \"key\"  :-1002}"
        `shouldParse` "{\"key\":-1002}"
  describe "Invalid JSON" $
    it "Should fail validation with unquoted key" $
      M.parse jsonParser ""
        `shouldFailOn` "{key:\"value\"}"
  describe "Invalid JSON" $
    it "Should fail validation with missing key" $
      M.parse jsonParser ""
        `shouldFailOn` "{:\"value\"}"
  describe "Invalid JSON" $
    it "Should fail validation with missing value" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":}"
