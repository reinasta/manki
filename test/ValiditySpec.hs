{-# LANGUAGE NoImplicitPrelude #-}
module ValiditySpec (spec) where

import Prelude (repeat,concat,head,and,putStrLn)
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Import hiding (many,some,try)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Validity
import Parser
import Types
import Generators
import Util (isRow, isCell,(<+>),numCells,numRows,numRowsAndCells)
import Write (stringifyBack)
import Schema (getSchemaString,genFromSchemaCSV)

spec :: Spec
spec = do


  describe "withRowsWithout" $ do

    it "lists rows/blocks with the wrong number of cells/fields" $ do
      let badCsv =  "this is ~a~ csv string *but* it's bad,\n\n unfortunately" :: String

      let res = parse csv "" badCsv
      let Right badRows = fmap (\x -> x `withRowsWithout` 3) res

      badRows `shouldSatisfy` (not . null)

    it "if a csv has all rows of the same dimensions, its constituents have the same property" $
      quickCheck $ \c1 c2 -> equalRows (c1 <+> c2) ==> (equalRows c1 && equalRows c2)

    it "at least some randomly generated CSVs should have unequal rows" $ do
      cs <- sample' arbitrary :: IO [CSV]
      and (fmap equalRows cs) `shouldBe` False

    it "detects CSVs with equal and unequal rows" $ do

      let csv_eq = Csv [ Row [Cell [Bold "one"], Cell [Bold "two"]]
                       , Row [Cell [Italic "one"], Cell [Italic "two"]]
                       , Row [Cell [Regular "one"], Cell [Regular "two"]]
                       ]

      let csv_uneq = Csv [ Row [Cell [Bold "one"], Cell [Bold "two"]]
                         , Row [Cell [Italic "one"], Cell [Italic "two"], Cell [Italic "three, ouch!"]]
                         , Row [Cell [Regular "one"], Cell [Regular "two"]]
                         ]

      equalRows csv_eq `shouldBe` True
      equalRows csv_uneq `shouldBe` False


    it "checks that rows don't mix with cells under Csv constructor" $ do

      -- checks whethe there are any cells under Csv, where only rows are allowed
      let noCells c = case c of
                        (Csv rs) -> not $ any isCell rs
                        _        -> True
      let prop_no_cells_among_rows :: CSV -> Bool
          prop_no_cells_among_rows c =
            case parse csv "" (stringifyBack c) of
              Right c' -> noCells c'
              _        -> False

      quickCheck $ withMaxSuccess 10 prop_no_cells_among_rows


    it "checks that every audio has a matching insert place, and vice-versa" $ do
      -- csv tree with matching audio - insert pairs
      let csv_ast = Csv [ Row [ Cell [Bold "ABC", Audio [] 31 [], Audio [] 32 [], Bold "B", AudioInsert 6 [] ]
                              , Cell [Bold "#35a", AudioInsert 31 [], Bold "blah", AudioInsert 45 []]
                              , Cell [Bold "Foo", AudioInsert 32 [], Italic "C", Audio [] 45 [], Audio [] 6 []]
                              ]
                        , Row [ Cell [Bold "blah", Audio [] 1 [], Audio [] 2 [Italic "f"], Bold "RFG"]
                              , Cell [Bold "blah", AudioInsert 1 [], Regular "#1", AudioInsert 2 [Bold "K"]]
                              , Cell [Bold "blah", AudioInsert 1 [Cloze 5 ""], Audio [Invisible] 1 [] ]
                              ]
                        , Row [ Cell [Bold "blah", AudioInsert 9999 [], AudioInsert 12 [], AudioInsert 7 []]
                              , Cell [Bold "blah", Audio [Visible] 666 [], AudioInsert 666 [Italic "it"]]
                              , Cell [Bold "blah", Audio [] 9999 [], Bold "", Audio [] 12 [], Audio [] 7 []]
                              ]
                        ]

      -- csv strings with matching audio - insert pairs
      let row_str1 = "@a@1, @b@2, @c@3\n--\n1|>2|>3|>blah\n--\n@d@4 blah 4|>, @zero@, |>"
      let row_str2 = "@a@11, @b@12, @c@13\n--\n11|>12|>13|>blah\n--\n@d@14 blah 14|>, @zero@, |>"
      let row_str3 = "13|>@f@7,@g@9,@h@11,@i@13\n--\n11|> blah\n--\n9|>blah7|> and finally 22|> 23|> @22@22 @23@23"
      let manki_str = row_str1 ++ row_str2 ++ row_str3

      audioInsertMatching csv_ast `shouldBe` True
      let Right csv' = parse (csvWith $ rowWith id) "" manki_str in audioInsertMatching csv' `shouldBe` True


    it "checks that every audio has a matching insert place, and vice-versa (schema version)" $ do
      -- many-to-one relations are allowed, in both senses: A -> Is, I -> As
      csv2x3_unmatched <- genFromSchemaCSV 2 3 "A1 C10 I2 A2 C12 I1"
      csv2x3_matched <- genFromSchemaCSV 2 3 "A1 C10 I1 I2 C12 A2"
      csv2x4_unmatched <- genFromSchemaCSV 2 4 "A1 C1 I1 I2 I2 C1 A2 A2"
      csv2x4_matched <- genFromSchemaCSV 2 4 "I1 C1 C2 A1 A2 A2 I2 C7"

      audioInsertMatching csv2x3_unmatched `shouldBe` False
      audioInsertMatching csv2x3_matched `shouldBe` True
      audioInsertMatching csv2x4_unmatched `shouldBe` False
      audioInsertMatching csv2x4_matched `shouldBe` True


    it "checks that every audio has a exactly one matching insert place, and vice-versa (schema version)" $ do
      -- only a one-to-one relation btw audios and inserts is allowed
      csv2x3_unmatched <- genFromSchemaCSV 2 3 "A1 A2 I1 A2 C7 I2"
      csv2x3_matched <- genFromSchemaCSV 2 3 "A1 C1 I1 I2 C12 A2"
      csv2x4_unmatched <- genFromSchemaCSV 2 4 "A1 A2 I1 I2 I2 C1 A2 A2"
      csv2x4_matched <- genFromSchemaCSV 2 4 "I1 A2 I2 A1 A2 A2 I2 I2"

      audioInsertMatching' csv2x3_unmatched `shouldBe` False
      audioInsertMatching' csv2x3_matched `shouldBe` True
      audioInsertMatching' csv2x4_unmatched `shouldBe` False
      audioInsertMatching' csv2x4_matched `shouldBe` True

