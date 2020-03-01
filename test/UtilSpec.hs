{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec (spec) where

import Prelude (repeat,concat,head)
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Import hiding (many,some,try)
import Test.Hspec
import Test.Hspec.QuickCheck

import Util
import Parser
import Types
import Generators


spec :: Spec
spec = do
  describe "Util checks" $ do

    prop "isDefaultMarkup" $ \ms ->
      let defs' = Audio [] 0 [] : []
          moreThanZeroDefaults = length (filter isDefaultMarkup (ms ++ defs')) > 0
      in moreThanZeroDefaults `shouldBe` True

    it "addCSV: adds CSV elements" $ do
      let csv1 = Csv [Row [Cell [Regular "ROW ONE",Bold "CELL ONE"]]]
      let cell2 = Cell [Bold "CELL TWO"]
      let row2 = Row [Cell [Italic "IN ROW 2"]]
      let csv2 = Csv [Row [Cell [Regular "CSV TWO"]]]

      let csv1_cell2 = Csv [Row [ Cell [Regular "ROW ONE",Bold "CELL ONE"]
                                , Cell [Bold "CELL TWO"]]]
      let csv1_row2 = Csv [ Row [Cell [Regular "ROW ONE",Bold "CELL ONE"]]
                          , Row [Cell [Italic "IN ROW 2"]]]
      let cell2_row2 = Row [ Cell [Bold "CELL TWO"]
                           , Cell [Italic "IN ROW 2"]
                           ]
      let csv1_csv2 = Csv [ Row [Cell [Regular "ROW ONE",Bold "CELL ONE"]]
                          , Row [Cell [Regular "CSV TWO"]]
                          ]

      csv1 <+> cell2 `shouldBe` csv1_cell2
      csv1 <+> row2 `shouldBe` csv1_row2
      cell2 <+> row2 `shouldBe` cell2_row2
      csv1 <+> csv2 `shouldBe` csv1_csv2

    it "gets CSV element out of a larger CSV" $ do

      let csvex = Csv [ Row [Cell [Bold "row one, cell one"]]
                      , Row [Cell [Italic "row two, cell one"],Cell [Italic "row two, cell two"]]
                      , Row [Cell [Regular "row three, cell one"], Cell [Bold "row three, cell two"]]
                      ]
      let csvex_cell1 = getCellNum 1 csvex
      let csvex_row2 = getRowNum 2 csvex
      let csvex_row3_cell2 = getRowCell 3 2 csvex
      let csvex_nonexistent2 = getRowNum 4 csvex
      let csvex_nonexistent33 = getRowCell 3 3 csvex

      csvex_cell1 `shouldBe` Just (Cell [Bold "row one, cell one"])
      csvex_row2 `shouldBe` Just (Row [Cell [Italic "row two, cell one"],Cell [Italic "row two, cell two"]])
      csvex_row3_cell2 `shouldBe` Just (Cell [Bold "row three, cell two"])
      csvex_nonexistent2 `shouldBe` Nothing
      csvex_nonexistent33 `shouldBe` Nothing



    it "addCSV: adds three CSV elements" $ do

      let add3 = Csv [Row [Cell [Regular "ROW ONE",Bold "CELL ONE"]]] 
            <+> Cell [Bold "CELL TWO"]
            <+> Row [Cell [Italic "IN ROW 2"]]
      let add3res = Csv [Row [Cell [Regular "ROW ONE",Bold "CELL ONE"]
                             ,Cell [Bold "CELL TWO"]
                             ]
                        ,Row [Cell [Italic "IN ROW 2"]]]

      add3 `shouldBe` add3res


    prop "unwrapCSV: shallow unwrap of CSV elements (does not further unwrap Audios and AudioInserts)" $
      \c1@(Csv _) c2@(Csv _) ->
        let count' c@(Csv _) = length $ unwrapCSV c
        in count' c1 + count' c2 `shouldBe` count' (c1 <+> c2)


    prop "numRowsAndCells" $ \c1@(Csv _) c2@(Csv _) ->
      let addtup (x1,y1) (x2,y2) = (x1+x2,y1+y2)
      in addtup (numRowsAndCells c1) (numRowsAndCells c2)
           `shouldBe` (numRowsAndCells $ c1 <+> c2)

    -- number of indexed and non-indexed markups is the total number of markups
    prop "#indexed_markups + #unindexed_markups = #markups" $ \c ->
      let all_markups = flattenAudios c
          num_indexed_markups = length $ filter isIndexable all_markups 
          num_unindexed_markups = length $ filter (not . isIndexable) all_markups
      in lengthCSV c `shouldBe` num_indexed_markups + num_unindexed_markups

