module ValidateArraySpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a JSON with an array of strings value" $
      M.parse jsonParser "" "{\"key\":[\"one\",\"two\"]}"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"
  describe "Valid JSON" $
    it "Should validate a JSON with an array of intergers value" $
      M.parse jsonParser "" "{\"key\":[123,456]}"
        `shouldParse` "{\"key\":[123,456]}"
  describe "Valid JSON" $
    it "Should validate a JSON with an array leading space" $
      M.parse jsonParser "" "{\"key\":    [\"one\",\"two\"]}"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"
  describe "Valid JSON" $
    it "Should validate a JSON with an array trailing space" $
      M.parse jsonParser "" "{\"key\":[\"one\",\"two\"]    }"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"
  describe "Valid JSON" $
    it "Should validate a JSON with an array leading and trailing space" $
      M.parse jsonParser "" "{\"key\":    [\"one\",\"two\"]    }"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"
  describe "Invalid JSON" $
    it "Should fail validation with missing comma" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":[\"one\"\"two\"]}"
