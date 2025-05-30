{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemaSpec (spec) where

import Prelude
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Util
import Write
import Types
import Generators
import Schema

spec :: Spec
spec = do
  describe "audioSchematon" $ do

    it "parses an audio schematon string: A999" $ do
      parse audioSchematon "" "A999" `shouldParse` (A 999)

    it "parses schemata string" $ do
      parse schemata "" "A1 I3 A1 I3 C2 A1 I4 C2" `shouldParse`
        [A 1, I 3, A 1, I 3, C 2, A 1, I 4, C 2]

    it "from a schema string, create a csv of two rows, each having 2 cells" $ do
      csv2x4 <- genFromSchemaCSV 2 4 "A1 I1 I2 I3 A3 A2 A4 I4"
      numRowsAndCells csv2x4 `shouldBe` (2,8)

    it "from schema string to 2x4 csv and back" $ do
      let schema8_str0 = "A1 I3 A1 I3 C2 A1 I4 C2"
      csv2x4 <- genFromSchemaCSV 2 4 schema8_str0
      let schema8_str1 = getSchemaString csv2x4
      schema8_str0 `shouldBe` schema8_str1

    it "from schema string to 3x3 csv and back" $ do
      csv3x3 <- genFromSchemaCSV 3 3 "A1 I3 A1 I3 C2 A1 I4 C2 I77"
      getSchemaString csv3x3 `shouldBe` "A1 I3 A1 I3 C2 A1 I4 C2 I77"

