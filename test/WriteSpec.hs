{-# LANGUAGE NoImplicitPrelude #-}
module WriteSpec (spec) where

import Prelude (repeat,concat,head)
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Import hiding (many,some,try)
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List (intersperse)

import Util
import Write
import Types


spec :: Spec
spec = do
  describe "stringify" $ do

    it "stringifies audio insert" $ do
      let insert = AudioInsert 8 [Regular "  one\n two\n three ", Cloze 3 "four", Bold " five! "]
      -- NB: the 'four' and ' five' elements get an extra space from stringifyElem!
      let insert_res_1 = "  one\n two\n three  four  five! "
      let insert_res_2 = "[sound:one_two_three_four_five.mp3]"
      stringifyElem insert `shouldBe` insert_res_1
      ankiStringifyElem insert `shouldBe` insert_res_2

    it "stringifies audio insert with a more idiosyncratic use of white space" $ do
      let insert = AudioInsert 8 [Regular "  one\ntwo\nthree ", Cloze 3 "four  ", Bold " five! "]
      -- NB: the 'four  ' and '  five' elements get an extra space from stringifyElem!
      let insert_res_1 = "  one\ntwo\nthree  four    five! "
      let insert_res_2 = "[sound:one_two_three_four_five.mp3]"
      stringifyElem insert `shouldBe` insert_res_1
      ankiStringifyElem insert `shouldBe` insert_res_2


    it "strigifies markups" $ do
      let string_res = "this 1st or ups 2nd audio string ups!"
      let mkups =
                [ Cloze 1 "this 1st"
                , Regular " or "
                , Italic "ups"
                , Regular " "
                , Cloze 3 "2nd"
                , Regular " "
                , Audio [] 5 [Regular "audio ", Cloze 3 "string"]
                , Regular " "
                , Bold "ups"
                , Regular "!"
                ]
      concatMap stringifyElem mkups `shouldBe` string_res

    it "strigifies markup list with invisible elements" $ do
      let string_res = "this misses: ... ups 2nd missing ... ups!"
      let mkups =
                [ Cloze 1 "this misses:"
                , Audio [Invisible] 5 [Regular "missing string"]
                , Italic " ... ups"
                , Regular " "
                , Cloze 3 "2nd"
                , Regular " missing ..."
                , Audio [Invisible] 5 [Regular "second missing "]
                , Regular " "
                , Bold "ups"
                , Regular "!"
                ]
      concatMap stringifyElem mkups `shouldBe` string_res


    it "strigifies csv tree to csv string" $ do
      let cell1 = Cell [Regular "one ", Cloze 0 "\"two\"", Audio [] 3 [Bold " three"]]
      let cell2 = Cell [Regular "four ", Audio [] 3 [Bold " five ", Cloze 1 "six"]]
      let cell3 = Cell [Bold "enough ", Cloze 2 "is", Audio [] 6 [Italic " enough"]]
      let row1 = Row [cell1, cell2, cell3]
      let csv1 = Csv $ take 3 $ repeat row1
      let csv1_string = concat $ intersperse "\n" $ take 3 $ repeat $
                      "\"one {{c0::\"\"two\"\"}}<b> three</b>\","
                      ++ "\"four <b> five </b>{{c1::six}}\","
                      ++ "\"<b>enough </b>{{c2::is}}<i> enough</i>\"" :: String
      ankiStringifyCSV csv1 `shouldBe` csv1_string

  describe "stringify for anki" $ do

    it "audioFilename: remove punctuation and replace white space with underscore" $ do

      let s1 = ",a,b.;c!d 4.5#!" :: String
      let s1_aud = "abcd_45" :: String

      let s2 = ",a,b...c" :: String
      let s2_aud = "abc" :: String

      let s3 = "#1,#2,a,b.C;!? <end>" :: String
      let s3_aud = "12abC_end" :: String

      let s4 = "a/b, c\\d" :: String
      let s4_aud = "ab_cd" :: String

      let s5 = "\"double-quoted\", \'single-quoted\'! " :: String
      let s5_aud = "doublequoted_singlequoted" :: String

      audioFilename s1 `shouldBe` s1_aud
      audioFilename s2 `shouldBe` s2_aud
      audioFilename s3 `shouldBe` s3_aud
      audioFilename s4 `shouldBe` s4_aud
      audioFilename s5 `shouldBe` s5_aud

    it "anki markup for audio filename (extracted from insert)" $ do

      let sound_wrap str = "[sound:" ++ str ++ ".mp3]"

      let a1 = AudioInsert 1 [Regular "a.b.c.--e.f.g. ha!"]
      let a1_anki = sound_wrap "abcefg_ha"

      let a2 = AudioInsert 2 [Italic "abc!! efg: ha ;"]
      let a2_anki = sound_wrap "abc_efg_ha"

      let a3 = AudioInsert 3 [Cloze 756 "# t?r?e ? f!r!a ! g::h::c "]
      let a3_anki = sound_wrap "tre_fra_ghc"

      ankiStringifyElem a1 `shouldBe` a1_anki
      ankiStringifyElem a2 `shouldBe` a2_anki
      ankiStringifyElem a3 `shouldBe` a3_anki

    it "anki markup for MathInline" $ do
      let math_inline_input = MathInline "x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}"
      let math_inline_expected = "<anki-mathjax>x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}</anki-mathjax>"
      ankiStringifyElem math_inline_input `shouldBe` math_inline_expected

    it "anki markup for MathInline with characters requiring csvFilters" $ do
      let math_inline_input_csv = MathInline "P(\"event\") < 0.5\nNext line"
      let math_inline_expected_csv = "<anki-mathjax>P(\"\"event\"\") < 0.5<br>Next line</anki-mathjax>"
      ankiStringifyElem math_inline_input_csv `shouldBe` math_inline_expected_csv

    it "anki markup for MathBlock" $ do
      let math_block_input = MathBlock "f(x) = \\int_{-\\infty}^x e^{-t^2/2} dt"
      let math_block_expected = "<anki-mathjax block=\"true\">f(x) = \\int_{-\\infty}^x e^{-t^2/2} dt</anki-mathjax>"
      ankiStringifyElem math_block_input `shouldBe` math_block_expected

    it "anki markup for MathBlock with characters requiring csvFilters" $ do
      let math_block_input_csv = MathBlock "A = \"Some matrix\"\n\\[ \\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix} \\]"
      let math_block_expected_csv = "<anki-mathjax block=\"true\">A = \"\"Some matrix\"\"<br>\\[ \\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix} \\]</anki-mathjax>"
      ankiStringifyElem math_block_input_csv `shouldBe` math_block_expected_csv

