module ValidateNumberSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import ValidateNumber (numberValueParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a JSON with number value" $
      M.parse numberValueParser "" "12345"
        `shouldParse` "12345"
  describe "Valid JSON" $
    it "Should validate a JSON with zero number value" $
      M.parse numberValueParser "" "0"
        `shouldParse` "0"
  describe "Valid JSON" $
    it "Should validate a JSON with negative number value" $
      M.parse numberValueParser "" "-1002"
        `shouldParse` "-1002"
  describe "Valid JSON" $
    it "Should validate a JSON with negative zeron number value" $
      M.parse numberValueParser "" "-0"
        `shouldParse` "-0"
