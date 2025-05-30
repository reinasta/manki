{-# LANGUAGE NoImplicitPrelude #-}
module InterfaceSpec (spec) where

import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


import Types
import Util
import Interface
import Schema


spec :: Spec
spec = do
  describe "selectUpdate" $ do

    it "selectUpdate should select (rewriteIndices . copyUnmergedAudios)" $ do
      csv2x6 <- genFromSchemaCSV 2 6 "A0 A3 A0 I0 I0 I3 A0 A5 A0 I0 I0 I5"
      --numRowsAndCells csv2x6 `shouldBe` (1,6)
      let csv2x6_str = getSchemaString $ selectUpdate csv2x6
      csv2x6_str `shouldBe` "A1 A2 A1 I1 I1 I2 A1 A2 A1 I1 I1 I2"

    it "selectUpdate should select copyAudios (without any index update)" $ do
      csv1x6 <- genFromSchemaCSV 1 6 "A0 A1 A2 I0 I1 I2"
      --numRowsAndCells csv1x6 `shouldBe` (1,6)
      let csv1x6_str = getSchemaString csv1x6
      csv1x6_str `shouldBe` "A0 A1 A2 I0 I1 I2"
      -- NB: all the audios should be in the same row, since the update takes place at row level



{-

A0 A3 A0 I0 I0 I3 -> A0 A3 A0 I0 I0 I3

A0 A1 A2 I0 I1 I2 -> A0 A1 A2 I0 I1 I2

-}
