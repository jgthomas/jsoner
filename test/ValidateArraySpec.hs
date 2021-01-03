module ValidateArraySpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a JSON with an array of string values" $
      M.parse jsonParser "" "{\"key\":[\"one\",\"two\"]}"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"

  describe "Valid JSON" $
    it "Should validate a JSON with an array of interger values" $
      M.parse jsonParser "" "{\"key\":[123,456]}"
        `shouldParse` "{\"key\":[123,456]}"

  describe "Valid JSON" $
    it "Should validate a JSON with an array of empty objects" $
      M.parse jsonParser "" "{\"key\":[{},{}]}"
        `shouldParse` "{\"key\":[{},{}]}"

  describe "Valid JSON" $
    it "Should validate a JSON with an array of objects with fields" $
      M.parse jsonParser "" "{\"key\":[{\"key\":123},{\"key\":456}]}"
        `shouldParse` "{\"key\":[{\"key\":123},{\"key\":456}]}"

  describe "Valid JSON" $
    it "Should validate a JSON with an array of empty arrays" $
      M.parse jsonParser "" "{\"key\":[[],[]]}"
        `shouldParse` "{\"key\":[[],[]]}"

  describe "Valid JSON" $
    it "Should validate a JSON with an array of integer arrays" $
      M.parse jsonParser "" "{\"key\":[[1,2,3],[4,5,6]]}"
        `shouldParse` "{\"key\":[[1,2,3],[4,5,6]]}"

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

  describe "Valid JSON" $
    it "Should validate a JSON with an array space between elements" $
      M.parse jsonParser "" "{\"key\":[\"one\"  ,  \"two\"]}"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"

  describe "Valid JSON" $
    it "Should validate a JSON with an array with space before first element" $
      M.parse jsonParser "" "{\"key\":[   \"one\",\"two\"]}"
        `shouldParse` "{\"key\":[\"one\",\"two\"]}"

  describe "Invalid JSON" $
    it "Should fail validation with missing comma" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":[\"one\"\"two\"]}"

  describe "Invalid JSON" $
    it "Should fail validation with comma after final value" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":[\"one\",\"two\",]}"

  describe "Invalid JSON" $
    it "Should fail validation with missing open bracket" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"one\",\"two\"]}"

  describe "Invalid JSON" $
    it "Should fail validation with missing close bracket" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":[\"one\",\"two\"}"

  describe "Invalid JSON" $
    it "Should fail validation with missing both brackets" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"one\",\"two\"}"
