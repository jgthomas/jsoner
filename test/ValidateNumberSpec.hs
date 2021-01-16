module ValidateNumberSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a JSON with number value" $
      M.parse jsonParser "" "{\"key\":12345}"
        `shouldParse` "{\"key\":12345}"

  describe "Valid JSON" $
    it "Should validate a JSON with ZERO number value" $
      M.parse jsonParser "" "{\"key\":0}"
        `shouldParse` "{\"key\":0}"

  describe "Valid JSON" $
    it "Should validate a JSON with negative zero number value" $
      M.parse jsonParser "" "{\"key\":-0}"
        `shouldParse` "{\"key\":-0}"

  describe "Valid JSON" $
    it "Should validate a JSON with NEGATIVE number value" $
      M.parse jsonParser "" "{\"key\":-1002}"
        `shouldParse` "{\"key\":-1002}"

  describe "Valid JSON" $
    it "Should validate a JSON with positive exponent number value" $
      M.parse jsonParser "" "{\"key\":1e+10}"
        `shouldParse` "{\"key\":1e+10}"

  describe "Valid JSON" $
    it "Should validate a JSON with negative exponent number value" $
      M.parse jsonParser "" "{\"key\":1e-10}"
        `shouldParse` "{\"key\":1e-10}"

  describe "Valid JSON" $
    it "Should validate a JSON with negative coefficient and exponent" $
      M.parse jsonParser "" "{\"key\":-1e-10}"
        `shouldParse` "{\"key\":-1e-10}"

  describe "Valid JSON" $
    it "Should validate a JSON with leading zeros exponent number value" $
      M.parse jsonParser "" "{\"key\":-1e-00010}"
        `shouldParse` "{\"key\":-1e-00010}"

  describe "Valid JSON" $
    it "Should validate if multidigit coeffcient" $
      M.parse jsonParser "" "{\"key\":1000e-00010}"
        `shouldParse` "{\"key\":1000e-00010}"

  describe "Valid JSON" $
    it "Should validate hex value letters lower case" $
      M.parse jsonParser "" "{\"key\":\"\\uabcdef\"}"
        `shouldParse` "{\"key\":\"\\uabcdef\"}"

  describe "Valid JSON" $
    it "Should validate hex value letters upper case" $
      M.parse jsonParser "" "{\"key\":\"\\uABCDEF\"}"
        `shouldParse` "{\"key\":\"\\uABCDEF\"}"

  describe "Valid JSON" $
    it "Should validate hex value letters mixed case" $
      M.parse jsonParser "" "{\"key\":\"\\uAbCdeF\"}"
        `shouldParse` "{\"key\":\"\\uAbCdeF\"}"

  describe "Valid JSON" $
    it "Should validate hex value letters lower case and numbers" $
      M.parse jsonParser "" "{\"key\":\"\\ua0cd1f\"}"
        `shouldParse` "{\"key\":\"\\ua0cd1f\"}"

  describe "Valid JSON" $
    it "Should validate hex value letters upper case and numbers" $
      M.parse jsonParser "" "{\"key\":\"\\u12CDE0\"}"
        `shouldParse` "{\"key\":\"\\u12CDE0\"}"

  describe "Valid JSON" $
    it "Should validate hex value letters mixed case and numbers" $
      M.parse jsonParser "" "{\"key\":\"\\uA0Cd1F\"}"
        `shouldParse` "{\"key\":\"\\uA0Cd1F\"}"

  describe "Invalid JSON" $
    it "Should fail validation with positive zero number value" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":+0}"

  describe "Invalid JSON" $
    it "Should fail validation if using positive sign with positive number" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":+1000}"

  describe "Invalid JSON" $
    it "Should fail validation with explicitly positive coefficient" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":+1e+10}"

  describe "Invalid JSON" $
    it "Should fail validation if multiple zeros" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":000}"

  describe "Invalid JSON" $
    it "Should fail validation if multiple zeros with following number" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":0001}"

  describe "Valid JSON" $
    it "Should fail validation invalid hex letters" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"\\uz0Cd1F\"}"
