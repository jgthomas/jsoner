module ValidateStringSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate with a string value" $
      M.parse jsonParser "" "{\"key\":\"value\"}"
        `shouldParse` "{\"key\":\"value\"}"

  describe "Valid JSON" $
    it "Should validate with an escaped forward slash" $
      M.parse jsonParser "" "{\"key\":\"val\\/ue\"}"
        `shouldParse` "{\"key\":\"val\\/ue\"}"

  describe "Valid JSON" $
    it "Should validate with an escaped back slash" $
      M.parse jsonParser "" "{\"key\":\"val\\\\ue\"}"
        `shouldParse` "{\"key\":\"val\\\\ue\"}"

  describe "Valid JSON" $
    it "Should validate with an escaped double quote" $
      M.parse jsonParser "" "{\"key\":\"val\\\"ue\"}"
        `shouldParse` "{\"key\":\"val\\\"ue\"}"

  describe "Inalid JSON" $
    it "Should fail validation when missing opening quote" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":value\"}"

  describe "Inalid JSON" $
    it "Should fail validation when missing closing quote" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"value}"

  describe "Inalid JSON" $
    it "Should fail validation with unescaped forward slash" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"val/ue\"}"

  describe "Inalid JSON" $
    it "Should fail validation with unescaped back slash" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"val\\ue\"}"

  describe "Inalid JSON" $
    it "Should fail validation with unescaped double quote" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"va\"ue\"}"
