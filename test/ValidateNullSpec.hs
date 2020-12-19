module ValidateNullSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a null value" $
      M.parse jsonParser "" "{\"key\":null}"
        `shouldParse` "{\"key\":null}"
  describe "Valid JSON" $
    it "Should validate a null value leading space" $
      M.parse jsonParser "" "{\"key\":  null}"
        `shouldParse` "{\"key\":null}"
  describe "Valid JSON" $
    it "Should validate a null value trailing space" $
      M.parse jsonParser "" "{\"key\":  null  }"
        `shouldParse` "{\"key\":null}"
  describe "Invalid JSON" $
    it "Should fail validation with mixed case" $
      M.parse jsonParser "{\"key\":nUll}"
        `shouldFailOn` "U"
  describe "Invalid JSON" $
    it "Should fail validation with upper case" $
      M.parse jsonParser "{\"key\":NULL}"
        `shouldFailOn` "N"
