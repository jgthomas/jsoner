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

  describe "Valid JSON" $
    it "Should validate with a hex number" $
      M.parse jsonParser "" "{\"key\":\"value\\uAABB\"}"
        `shouldParse` "{\"key\":\"value\\uAABB\"}"

  describe "Valid JSON" $
    it "Should validate with a hex number followed by non-hex" $
      M.parse jsonParser "" "{\"key\":\"value\\uAABBzzxx\"}"
        `shouldParse` "{\"key\":\"value\\uAABBzzxx\"}"

  describe "Valid JSON" $
    it "Should validate with just a hex number" $
      M.parse jsonParser "" "{\"key\":\"\\uAABB\"}"
        `shouldParse` "{\"key\":\"\\uAABB\"}"

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

  describe "Valid JSON" $
    it "Should fail validation with a short hex number" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"value\\uAAB\"}"

  describe "Valid JSON" $
    it "Should fail validation with non-hex number" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":\"value\\uZZXX\"}"
