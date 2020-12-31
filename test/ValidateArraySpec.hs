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
