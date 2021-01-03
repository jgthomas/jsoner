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
    it "Should validate a JSON with empty value" $
      M.parse jsonParser "" "{\"key\":\"\"}"
        `shouldParse` "{\"key\":\"\"}"

  describe "Valid JSON" $
    it "Should validate a JSON key with leading space" $
      M.parse jsonParser "" "{   \"key\":-1002}"
        `shouldParse` "{\"key\":-1002}"

  describe "Valid JSON" $
    it "Should validate a JSON key with trailing space" $
      M.parse jsonParser "" "{\"key\"  :-1002}"
        `shouldParse` "{\"key\":-1002}"

  describe "Valid JSON" $
    it "Should validate a JSON with leading space top-level object" $
      M.parse jsonParser "" "     {\"key\":-1002}"
        `shouldParse` "{\"key\":-1002}"

  describe "Valid JSON" $
    it "Should validate a JSON with trailing space top-level object" $
      M.parse jsonParser "" "{\"key\":-1002}     "
        `shouldParse` "{\"key\":-1002}"

  describe "Valid JSON" $
    it "Should validate a JSON key with leading and trailing space" $
      M.parse jsonParser "" "{   \"key\"  :-1002}"
        `shouldParse` "{\"key\":-1002}"

  describe "Valid JSON" $
    it "Should validate a JSON with multiple values" $
      M.parse jsonParser "" "{\"key1\":\"value\",\"key2\":12345}"
        `shouldParse` "{\"key1\":\"value\",\"key2\":12345}"

  describe "Valid JSON" $
    it "Should validate a JSON with multiple key-value pairs, space between" $
      M.parse jsonParser "" "{\"key1\":\"value\"  ,  \"key2\":12345}"
        `shouldParse` "{\"key1\":\"value\",\"key2\":12345}"

  describe "Valid JSON" $
    it "Should validate a JSON with multiple values with array" $
      M.parse jsonParser "" "{\"key1\":\"value\",\"key2\":[1,2,3]}"
        `shouldParse` "{\"key1\":\"value\",\"key2\":[1,2,3]}"

  describe "Valid JSON" $
    it "Should validate a JSON with multiple values with object" $
      M.parse jsonParser "" "{\"key1\":\"value\",\"key2\":{\"key3\":123}}"
        `shouldParse` "{\"key1\":\"value\",\"key2\":{\"key3\":123}}"

  describe "Valid JSON" $
    it "Should validate a JSON with multiple values with object and space between" $
      M.parse jsonParser "" "{\"key1\":\"value\",\"key2\":   {\"key3\":123}}   "
        `shouldParse` "{\"key1\":\"value\",\"key2\":{\"key3\":123}}"

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
