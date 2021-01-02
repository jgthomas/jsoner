module ValidateBoolSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import Validate (jsonParser)

spec :: Spec
spec = do
  describe "Valid JSON" $
    it "Should validate a true value" $
      M.parse jsonParser "" "{\"key\":true}"
        `shouldParse` "{\"key\":true}"

  describe "Valid JSON" $
    it "Should validate a false value" $
      M.parse jsonParser "" "{\"key\":false}"
        `shouldParse` "{\"key\":false}"

  describe "Valid JSON" $
    it "Should validate a true value leading space" $
      M.parse jsonParser "" "{\"key\":  true}"
        `shouldParse` "{\"key\":true}"

  describe "Valid JSON" $
    it "Should validate a false value leading space" $
      M.parse jsonParser "" "{\"key\":  false}"
        `shouldParse` "{\"key\":false}"

  describe "Valid JSON" $
    it "Should validate a true value trailing space" $
      M.parse jsonParser "" "{\"key\":true  }"
        `shouldParse` "{\"key\":true}"

  describe "Valid JSON" $
    it "Should validate a false value trailing space" $
      M.parse jsonParser "" "{\"key\":false  }"
        `shouldParse` "{\"key\":false}"

  describe "Valid JSON" $
    it "Should validate a true value leading and trailing space" $
      M.parse jsonParser "" "{\"key\":  true  }"
        `shouldParse` "{\"key\":true}"

  describe "Valid JSON" $
    it "Should validate a false value leading and trailing space" $
      M.parse jsonParser "" "{\"key\":  false  }"
        `shouldParse` "{\"key\":false}"

  describe "Invalid JSON" $
    it "Should fail validation with true mixed case" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":tRue}"

  describe "Invalid JSON" $
    it "Should fail validation with false mixed case" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":fAlse}"

  describe "Invalid JSON" $
    it "Should fail validation with true upper case" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":TRUE}"

  describe "Invalid JSON" $
    it "Should fail validation with false upper case" $
      M.parse jsonParser ""
        `shouldFailOn` "{\"key\":FALSE}"
