module ValidateNumberSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import ValidateNumber (numberParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a JSON with number value" $
      M.parse numberParser "" "12345"
        `shouldParse` "12345"
  describe "Valid JSON" $
    it "Should validate a JSON with zero number value" $
      M.parse numberParser "" "0"
        `shouldParse` "0"
  describe "Valid JSON" $
    it "Should validate a JSON with negative number value" $
      M.parse numberParser "" "-1002"
        `shouldParse` "-1002"
  describe "Valid JSON" $
    it "Should validate a JSON with negative zero number value" $
      M.parse numberParser "" "-0"
        `shouldParse` "-0"
  describe "Valid JSON" $
    it "Should validate a JSON with negative exponent number value" $
      M.parse numberParser "" "1e-10"
        `shouldParse` "1e-10"
  describe "Valid JSON" $
    it "Should validate a JSON with explicit positive exponent number value" $
      M.parse numberParser "" "1e+10"
        `shouldParse` "1e+10"
  describe "Valid JSON" $
    it "Should validate a JSON with negative coefficient exponent number value" $
      M.parse numberParser "" "-1e-10"
        `shouldParse` "-1e-10"
  describe "Valid JSON" $
    it "Should validate a JSON with leading zeros exponent number value" $
      M.parse numberParser "" "-1e-00010"
        `shouldParse` "-1e-00010"
