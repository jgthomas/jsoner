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
  describe "Inalid JSON" $
    it "Should fail validation when missing opening quote" $
      M.parse stringParser ""
        `shouldFailOn` "value\""
  describe "Inalid JSON" $
    it "Should fail validation when missing closing quote" $
      M.parse stringParser ""
        `shouldFailOn` "\"value"
  describe "Inalid JSON" $
    it "Should fail validation with unescaped forward slash" $
      M.parse stringParser ""
        `shouldFailOn` "\"val/ue\""
  describe "Inalid JSON" $
    it "Should fail validation with unescaped back slash" $
      M.parse stringParser ""
        `shouldFailOn` "\"val\\ue\""
  describe "Inalid JSON" $
    it "Should fail validation with unescaped double quote" $
      M.parse stringParser ""
        `shouldFailOn` "\"va\"ue\""
