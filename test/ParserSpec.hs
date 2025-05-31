{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Prelude (repeat,concat,head,(!!),putStrLn)
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Data.Text as T (pack, count)
import Import hiding (many,some,try)
import Test.Hspec
import Test.Hspec.QuickCheck
--import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

import Parser
import Audio (copyAudios)
import Indices (updateIndices,rewriteIndices)
import Util (deleteWith,isInsert,unwrapCSV,numRowsAndCells,hasEmptyRows,takeCSV,hasEmptyElems)
import Write (stringifyBack)
import Types
import Generators


spec :: Spec
spec = do
  describe "makeSymmetricMarker" $ do

    it "makes a symmetric marker out of a symbol" $ do
      let dollar = makeSymmetricMarker "$"
      parse dollar "" "$" `shouldParse` "$"
      parse dollar "" `shouldFailOn` "blah"

  describe "doubleNewline" $ do

    it "parses two newline characters or more" $ do
      parse doubleNewline "" "\n\n" `shouldParse` "\n\n"
      parse doubleNewline "" "\r\n\r\n" `shouldParse` "\r\n\r\n"
      parse doubleNewline "" `shouldFailOn` "\nblah"
      parse doubleNewline "" `shouldFailOn` "\r\nblah"
      parse doubleNewline "" `shouldFailOn` "           " -- two tabs


  describe "wordChar" $ do

    it "parses characters that are not markers or part of special strings (e.g. double newlines) " $ do
      parse (some wordChar) "" "stop here |\n\n" `shouldParse` "stop here |"
      parse (some wordChar) "" "stop here <-\r\n\r\n" `shouldParse` "stop here <-"
      parse (some wordChar) "" `shouldSucceedOn` "\nblah"
      parse (some wordChar) "" `shouldSucceedOn` "\r\nblah"
      parse (some wordChar) "" `shouldSucceedOn` "           "
      parse (some wordChar) "" `shouldSucceedOn` "flah# #blah f|<>|f here"
      parse (some wordChar) "" `shouldSucceedOn` "here | blah"
      parse (some wordChar) "" `shouldSucceedOn` "don't fail *here* >|, please"
      parse (some wordChar) "" `shouldSucceedOn` "don't fail *here* |<, please"
      parse (some wordChar) "" `shouldSucceedOn` "don't fail 666, *please*"
      parse (some wordChar) "" `shouldSucceedOn` "don't fail on this ~ _please_"
      parse (some wordChar) "" "fail here @, I say" `shouldParse` "fail here "
      parse (some wordChar) "" "blah 666" `shouldParse` "blah 666"


  describe "bold' and italic'" $ do

    it "parses a bold string" $ do
      parse bold' "" "*bold string* blah" `shouldParse` "bold string"

    it "parses a bold string with symbols and newline characters" $ do
      parse bold' "" "*a\"\'\n4\\$%\n\\$^#\\$-!* blah" `shouldParse` "a\"\'\n4$%\n$^#$-!"

    it "parses an italic string" $ do
      parse italic' "" "_italic string_ blah" `shouldParse` "italic string"

    it "parses a non-italic string" $ do
      parse italic' "" `shouldFailOn` "non_italic string blah"

  describe "cloze" $ do

    it "parses a regular cloze deletion string" $ do
      parse cloze "" "~cloze deletion~ blah" `shouldParse` (Cloze 0 "cloze deletion")

    it "parses an explicitly indexed cloze deletion strings" $ do
      parse cloze "" "~cloze deletion~77 blah" `shouldParse` (Cloze 77 "cloze deletion")

    it "fails on strings without matching tildes" $ do
      parse cloze "" `shouldFailOn` "~not a cloze deletion string"

  describe "regular" $ do

    it "parses a string that is not marked up" $ do
      let gibberish = "#[\'foo \nbar\']&\n(\"foo-bar!\")"
      parse regular "" (gibberish ++ "*~_") `shouldParse` (Regular gibberish)

    it "fails on strings which begin with markers" $ do
      parse regular "" `shouldFailOn` "*blah"

  describe "mathInline" $ do

    it "parses basic inline math with dollar delimiters" $ do
      parse mathInline "" "$a^2 + b^2 = c^2$" `shouldParse` (MathInline "a^2 + b^2 = c^2")

    it "parses inline math with fractions and special characters" $ do
      parse mathInline "" "$\\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}$" `shouldParse` (MathInline "\\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}")

    it "fails on unclosed inline math" $ do
      parse mathInline "" `shouldFailOn` "$a+b"

  describe "mathBlock" $ do

    it "parses basic block math with double dollar delimiters" $ do
      parse mathBlock "" "$$E = mc^2$$" `shouldParse` (MathBlock "E = mc^2")

    it "parses block math with complex expressions" $ do
      parse mathBlock "" "$$\\sum_{i=1}^{n} x_i = \\int_0^1 f(x) dx$$" `shouldParse` (MathBlock "\\sum_{i=1}^{n} x_i = \\int_0^1 f(x) dx")

    it "fails on unclosed block math" $ do
      parse mathBlock "" `shouldFailOn` "$$a+b"

  describe "mixed math and text" $ do

    it "parses text with inline math" $ do
      let input = "The quadratic formula is $x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}$."
      let expected = [Regular "The quadratic formula is ", MathInline "x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}", Regular "."]
      parse (some markup) "" input `shouldParse` expected

    it "parses simple text with one inline math expression" $ do
      let input = "Value is $x$."
      let expected = [Regular "Value is ", MathInline "x", Regular "."]
      parse (some markup) "" input `shouldParse` expected

    it "parses inline math at the beginning of a string" $ do
      let input = "$x = 1$ is an equation."
      let expected = [MathInline "x = 1", Regular " is an equation."]
      parse (some markup) "" input `shouldParse` expected

    it "parses inline math at the end of a string" $ do
      let input = "An equation is $x = 1$."
      let expected = [Regular "An equation is ", MathInline "x = 1", Regular "."]
      parse (some markup) "" input `shouldParse` expected

    it "parses multiple inline math expressions consecutively" $ do
      let input = "$a$$b$"
      let expected = [MathInline "a", MathInline "b"]
      parse (some markup) "" input `shouldParse` expected

    it "parses inline math with escaped dollar signs within math content" $ do
      let input = "The conversion rate is $£1 = \\$1.35$, so $£10 = \\$13.5$."
      let expected = [Regular "The conversion rate is ", MathInline "£1 = \\$1.35", Regular ", so ", MathInline "£10 = \\$13.5", Regular "."]
      parse (some markup) "" input `shouldParse` expected

    it "parses text with escaped dollar signs outside of math" $ do
      let input = "This costs \\$5."
      let expected = [Regular "This costs $5."] -- \\$ in regular text becomes $
      parse (some markup) "" input `shouldParse` expected

    it "parses text with escaped dollar signs both inside and outside of math" $ do
      let input = "Price: \\$5, or $value \\approx \\$5$ in math."
      let expected = [Regular "Price: $5, or ", MathInline "value \\approx \\$5", Regular " in math."]
      parse (some markup) "" input `shouldParse` expected

    it "parses text with non-dollar-escaping backslashes in regular text and math" $ do
      let input = "Path: C:\\Users\\Name, formula $f(x) = x\\alpha + C:\\beta$"
      let expected = [Regular "Path: C:\\Users\\Name, formula ", MathInline "f(x) = x\\alpha + C:\\beta"]
      parse (some markup) "" input `shouldParse` expected



  describe "audioInsertMarker" $ do

    it "parses an audio insertion marker" $ do
      let cell1 = "blah *blah*\n foo bar |> = foo bar\n"
      let cell2 = "insert it here >|> _please_"
      let cell3 = "~insert it~ |> again |> _please_|>"

      let cell1_res = Cell [ Regular "blah "
                       , Bold "blah"
                       , Regular "\n foo bar "
                       , AudioInsert 0 []
                       , Regular " = foo bar\n"
                       ]

      let cell2_res = Cell [ Regular "insert it here >"
                           , AudioInsert 0 []
                           , Regular " "
                           , Italic "please"
                           ]

      let cell3_res = Cell [ Cloze 0 "insert it"
                           , Regular " "
                           , AudioInsert 0 []
                           , Regular " again "
                           , AudioInsert 0 []
                           , Regular " "
                           , Italic "please"
                           , AudioInsert 0 []
                           ]

      parse cell "" cell1 `shouldParse` cell1_res
      parse cell "" cell2 `shouldParse` cell2_res
      parse cell "" cell3 `shouldParse` cell3_res

  describe "mstrings" $ do

    it "parses many strings, marked up or not" $ do
      let mstring1 =
           "this ~first blah~ and _ups_ ~second blah~1 and that ~third blah~ ups *ups*!" :: String

      let parseMstring1 =
            [ Regular "this "
            , Cloze 0 "first blah"
            , Regular " and "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "second blah"
            , Regular " and that "
            , Cloze 0 "third blah"
            , Regular " ups "
            , Bold "ups"
            , Regular "!"
            ] :: [Markup]

      parse (markup `someTill` eof) "" mstring1 `shouldParse` parseMstring1

  describe "some expected (actually mandatory) markup parsing failures" $ do

    it "checks that field delimiters cannot be parsed as regular strings" $ do

        parse regular "" `shouldFailOn` "\n---\n"


  describe "cell" $ do

    it "parses many Markup strings up to a field delimiter" $ do
      let cellStr =
           "this ~first~ or _ups_ ~2nd~1 and that ~3rd blah~ ups *ups*!\n---\nSTOP" :: String

      let parseCell = Cell
            [ Regular "this "
            , Cloze 0 "first"
            , Regular " or "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " and that "
            , Cloze 0 "3rd blah"
            , Regular " ups "
            , Bold "ups"
            , Regular "!"
            ] :: Cell

      parse cell "" cellStr `shouldParse` parseCell

    it "parses many tricky Audio elements with dashes as content and invisibility markers (--@)" $ do
      let cellStr = "--@@--@@--@--@@@--@@" :: String

      let parseCell = Cell
            [ Audio [Invisible] 0 []
            , Audio [Invisible] 0 []
            , Audio [Invisible] 0 [Regular "--"]
            , Audio [Visible] 0 []
            , Audio [Invisible] 0 []
            ]

      parse cell "" cellStr `shouldParse` parseCell


  describe "row" $ do

    it "parses a row of three cells" $ do
      let cell1 =
           "this ~first~ or _ups_ ~2nd~1 and that ~3rd blah~ ups *ups*!\n---\n" :: String

      let cell2 =
           "this ~first~ or _ups_ ~2nd~1 and that ~3rd blah~ ups *ups*!\n---\n" :: String

      let cell3 =
           "this ~first~ or _ups_ ~2nd~1 and that ~3rd blah~ ups *ups*!" :: String

      let txtRow = cell1 ++ cell2 ++ cell3

      let parseCell = Cell
            [ Regular "this "
            , Cloze 0 "first"
            , Regular " or "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " and that "
            , Cloze 0 "3rd blah"
            , Regular " ups "
            , Bold "ups"
            , Regular "!"
            ] :: Cell

      let parseRow = Row $ take 3 (repeat parseCell)

      parse rowIgnoringIndices "" txtRow `shouldParse` parseRow

    it "parses several rows" $ do
      let cell1 =
           "this ~1st~ or\\$%^ _ups_ ~2nd~1 and-that ~3rd blah~ ups *ups*!\n---\n" :: String

      let cell2 =
           "this ~1st~1 and/or _ups_ ~2nd~1 foo--bar ~3rd blah~999 'ups'? * *!\n\n" :: String

      let txtRow = cell1 ++ cell1 ++ cell2
      let txtRows = concat $ take 5 $ repeat txtRow

      let parseCell1 = Cell
            [ Regular "this "
            , Cloze 0 "1st"
            , Regular " or$%^ "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " and-that "
            , Cloze 0 "3rd blah"
            , Regular " ups "
            , Bold "ups"
            , Regular "!"
            ] :: Cell

      let parseCell2 = Cell
            [ Regular "this "
            , Cloze 1 "1st"
            , Regular " and/or "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " foo--bar "
            , Cloze 999 "3rd blah"
            , Regular " 'ups'? "
            , Bold " "
            , Regular "!"
            ] :: Cell


      let parseRows = let r = Row [parseCell1, parseCell1, parseCell2]
                      in take 5 $ repeat r

      parse (some $ rowIgnoringIndices <* doubleNewline) "" txtRows `shouldParse` parseRows

  describe "audio" $ do
    it "parses @audio strings@" $ do
      let del = "\n---\n"
      let nls = "\n\n\n"

      let cell1 = "#this ~is~ *text* with @audio strings@\n phoa#"
      let cell2 = "*hey*: @audio string\n ~embedding cloze~@ @\nblah\n@x33"
      --let cell3 = "@Check@8 my email79@address.com @out@8, foo bar phoa?!"
      let cell3 = "@Check@8 this @out@8, foo bar phoa?!"

      let row1 = cell3 ++ del ++ cell1 ++ del ++ cell2 ++ nls
      let row2 = cell1 ++ del ++ cell2 ++ del ++ cell3 ++ nls
      let csv1 = concat $ take 2 $ repeat row1
      let csv2 = concat $ take 2 $ repeat row2

      let cell1_res = Cell [ Regular "#this "
                           , Cloze 0 "is"
                           , Regular " "
                           , Bold "text"
                           , Regular " with "
                           , Audio [Visible] 0 [Regular "audio strings"]
                           , Regular "\n phoa#"
                           ]

      let cell2_res = Cell [ Bold "hey"
                           , Regular ": "
                           , Audio [Visible] 0 [Regular "audio string\n ", Cloze 0 "embedding cloze"]
                           , Regular " "
                           , Audio [Visible] 0 [Regular "\nblah\n"]
                           , Regular "x33"
                           ]

      let cell3_res = Cell [ Audio [Visible] 8 [Regular "Check"]
                           , Regular " this "
                           --, Regular " my email79@address.com "
                           , Audio [Visible] 8 [Regular "out"]
                           , Regular ", foo bar phoa?!"
                           ]

      let row1_res = Row [cell3_res, cell1_res, cell2_res]
      let row2_res = Row [cell1_res, cell2_res, cell3_res]
      let csv1_res = Csv $ take 2 $ repeat row1_res
      let csv2_res = Csv $ take 2 $ repeat row2_res

      parse cell "" cell1 `shouldParse` cell1_res
      parse cell "" cell2 `shouldParse` cell2_res
      parse cell "" cell3 `shouldParse` cell3_res

      parse rowIgnoringIndices "" row1 `shouldParse` row1_res
      parse rowIgnoringIndices "" row2 `shouldParse` row2_res

      parse csvIgnoringIndices "" csv1 `shouldParse` csv1_res
      parse csvIgnoringIndices "" csv2 `shouldParse` csv2_res


    it "parses 'invisible' (-)-@audio strings@" $ do

      let cell1 = "here is a @visible@ and an -@invisible audio string@\n phoa!"
      let cell1_res = Cell [ Regular "here is a "
                           , Audio [Visible] 0 [Regular "visible"]
                           , Regular " and an "
                           , Audio [Invisible] 0 [Regular "invisible audio string"]
                           , Regular "\n phoa!"
                           ]

      parse cell "" cell1 `shouldParse` cell1_res

    it "tests that 'invisible' (-)-@audio strings@ do not interfere with block/cell delimiter" $ do

      let cell1 = "here is an \n--@invisible audio string@\n phoa!"
      let cell1_res = Cell [ Regular "here is an \n"
                           , Audio [Invisible] 0 [Regular "invisible audio string"]
                           , Regular "\n phoa!"
                           ]

      parse cell "" cell1 `shouldParse` cell1_res



  describe "csv" $ do

    it "parses a multi-row csv string with trailing newlines at the end" $ do
      let cell1 =
           "this ~1st~ or\\$%^ _ups_ ~2nd~1 and-that ~3rd blah~ ups *ups*!\n---\n" :: String

      let cell2 =
           "this ~1st~1 and/or _ups_ ~2nd~1 foo--bar ~3rd blah~999 'ups'? * *!\n\n" :: String

      let txtRow = cell1 ++ cell1 ++ cell2
      let txtRows = concat $ take 5 $ repeat txtRow

      let parseCell1 = Cell
            [ Regular "this "
            , Cloze 0 "1st"
            , Regular " or$%^ "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " and-that "
            , Cloze 0 "3rd blah"
            , Regular " ups "
            , Bold "ups"
            , Regular "!"
            ] :: Cell

      let parseCell2 = Cell
            [ Regular "this "
            , Cloze 1 "1st"
            , Regular " and/or "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " foo--bar "
            , Cloze 999 "3rd blah"
            , Regular " 'ups'? "
            , Bold " "
            , Regular "!"
            ] :: Cell


      let parseRows = let r = Row [parseCell1, parseCell1, parseCell2]
                      in Csv (take 5 $ repeat r)

      -- with trailing newlines at the end
      parse csvIgnoringIndices "" txtRows `shouldParse` parseRows


    it "parses a multi-row csv string without trailing newlines at the end" $ do
      let cell1 =
           "this ~1st~ or\\$%^ _ups_ ~2nd~1 and-that ~3rd blah~ ups *ups*!\n---\n" :: String

      let cell2 =
           "this ~1st~1 and/or _ups_ ~2nd~1 foo--bar ~3rd blah~999 'ups'? * *!\n\n" :: String

      let txtRow = cell1 ++ cell1 ++ cell2
      let txtRows = concat $ take 5 $ repeat txtRow

      let parseCell1 = Cell
            [ Regular "this "
            , Cloze 0 "1st"
            , Regular " or$%^ "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " and-that "
            , Cloze 0 "3rd blah"
            , Regular " ups "
            , Bold "ups"
            , Regular "!"
            ] :: Cell

      let parseCell2 = Cell
            [ Regular "this "
            , Cloze 1 "1st"
            , Regular " and/or "
            , Italic "ups"
            , Regular " "
            , Cloze 1 "2nd"
            , Regular " foo--bar "
            , Cloze 999 "3rd blah"
            , Regular " 'ups'? "
            , Bold " "
            , Regular "!"
            ] :: Cell


      let parseRows = let r = Row [parseCell1, parseCell1, parseCell2]
                      in Csv (take 5 $ repeat r)

      -- without trailing newlines
      parse csvIgnoringIndices "" (take (length txtRows - 2) txtRows) `shouldParse` parseRows

    it "parses a fully featured, multi-row csv string, dealing with indices" $ do
      let cell1 =
           "~this 1st~1 or _ups_ ~2nd~3 @audio ~string~3@5 *ups*!\n---\n" :: String

      let cell2 = -- issue: second insert shows up!
           "put audio string *here* 5|> *thank you* |> _ciao-ciao_!\n\n" :: String

      let row0 = cell1 ++ cell1 ++ cell2
      let rows0 = concat $ take 5 $ repeat row0

      let twoAudioMarkups = [Regular "audio ", Cloze 3 "string"]

      let parseCell1 = Cell
                     [ Cloze 1 "this 1st"
                     , Regular " or "
                     , Italic "ups"
                     , Regular " "
                     , Cloze 3 "2nd"
                     , Regular " "
                     , Audio [Visible] 5 twoAudioMarkups
                     , Regular " "
                     , Bold "ups"
                     , Regular "!"
                     ]

      let parseCell2 = Cell
                     [ Regular "put audio string "
                     , Bold "here"
                     , Regular " "
                     -- because coindexed audios are merged, and the row contains two Audio _ 5 _ elems
                     , AudioInsert 5 (twoAudioMarkups ++ twoAudioMarkups)
                     , Regular " "
                     , Bold "thank you"
                     , Regular " "
                     , Regular " " -- this double Regular space is due to a former insert that got deleted
                     , Italic "ciao-ciao"
                     , Regular "!"
                     ]

      let prow0 = Row [parseCell1, parseCell1, parseCell2]
      let prows0 = Csv $ take 5 $ repeat prow0


      parse row "" row0 `shouldParse` prow0
      parse csv "" rows0 `shouldParse` prows0


    it "parses a row string and moves one audio around (into an InsertAudio)" $ do

      let cell2 =
            "@audio which *does* get copied@5 |> *###1st default INSERT* |> _ciao_!" :: String
      let cell3 =
            "@audio which *surprisingly* gets copied@ 5|> *###2nd default INSERT* |> _ciao_!" :: String

      -- NB: roughly, updateIndices keeps default audios and inserts coindexed; that's why we get
      -- more co-indexed audio-insert pairs than the ones explicitly marked up (with index 5)

      let pcells = parse row "" $
            let del = "\n---\n"
            in cell2 ++ del ++ cell3 ++ del ++ cell2

      let threeMarkups = [ Regular "audio which ", Bold "does" , Regular " get copied"]


      let cell2_res = Cell [ Audio [Visible] 5 threeMarkups
                           , Regular " "
                           , Regular " "                     -- missing insert: assigned index 1
                           , Bold "###1st default INSERT"    -- gets deleted as insert below is
                           , Regular " "                     -- assigned 2 (instead of 1, as natural)
                           , AudioInsert 2 [ Regular "audio which "
                                           , Bold "surprisingly"
                                           , Regular " gets copied"
                                           ]
                           , Regular " "
                           , Italic "ciao"
                           , Regular "!"
                           ]

      let cell3_res = Cell [ Audio [Visible] 2 [ Regular "audio which "
                                               , Bold "surprisingly"
                                               , Regular " gets copied"
                                               ]
                           , Regular " "
                           -- duplicate markups due to there being two AudioInsert 5 _ which get merged
                           , AudioInsert 5 (threeMarkups ++ threeMarkups)
                           , Regular " "
                           , Bold "###2nd default INSERT"
                           , Regular " " -- the two Regular spaces were surrounding a now deleted insert 
                           , Regular " "
                           , Italic "ciao"
                           , Regular "!"
                           ]

      -- NB: copyAudio copies into the first AudioInsert 2 (in the first occurrence of cell2_res)
      -- and deletes the second (in the second occurrence of cell2_res)
      let res = Right $ Row [cell2_res, cell3_res, Cell (deleteWith isInsert 2 $ unwrapCSV cell2_res)]
      pcells `shouldBe` res


    it "parses a multi-row csv string with mixed indices, and copying default audio contents to different insert places" $ do

      let cell0 =
           "so: @###first non-indexed AUDIO@ foo @###second non-indexed AUDIO@\n---\n" :: String

      let cell1 =
           "~###first default INSERT:~ |> or _ups_ ~2nd~3 @audio ~str~3ing@5 *ups*!\n---\n" :: String

      let cell2 =
           "@audio which does *not* get copied@666 5|> *###2nd default INSERT* |> _ciao_!\n\n" :: String

      -- NB: first non-indexed audio is copied at the first, default insert marker

      let row0 = cell0 ++ cell1 ++ cell2
      let rows0 = concat $ take 5 $ repeat row0

      -- indices are difficult to predict so I just pasted the output-indices of a prior trial
      let parseCell0 = Cell
                     [ Regular "so: "
                     , Audio [Visible] 1 [Regular "###first non-indexed AUDIO"] -- former default
                     , Regular " foo "
                     , Audio [Visible] 1 [Regular "###second non-indexed AUDIO"] -- former default
                     ]

      let parseCell1 = Cell
                     [ Cloze 1 "###first default INSERT:"
                     , Regular " "
                     , AudioInsert 1 [Regular "###first non-indexed AUDIO"] -- former default
                     , Regular " or "
                     , Italic "ups"
                     , Regular " "
                     , Cloze 2 "2nd"
                     , Regular " "
                     , Audio [Visible] 2 [Regular "audio ", Cloze 2 "str", Regular "ing"] -- 5 -> 2
                     , Regular " "
                     , Bold "ups"
                     , Regular "!"
                     ]

      let parseCell2 = Cell
                     [ Audio [Visible] 3 [ Regular "audio which does "
                                         , Bold "not", Regular " get copied"] -- 666 -> 3
                     , Regular " "
                     , AudioInsert 2 [Regular "audio ", Cloze 2 "str", Regular "ing"]
                     , Regular " "
                     , Bold "###2nd default INSERT"
                     , Regular " "
                     , AudioInsert 1 [Regular "###second non-indexed AUDIO"] -- former default
                     , Regular " "
                     , Italic "ciao"
                     , Regular "!"
                     ]

      let prow0 = Row [parseCell0, parseCell1, parseCell2]
      let prows0 = Csv $ take 5 $ repeat prow0

      parse rowMultipleMixed "" row0 `shouldParse` prow0
      parse (csvWith rowMultipleMixed) "" rows0 `shouldParse` prows0

    it "checks that for nonempty CSVs, #cells = #delimiters + #rows" $ do
      quickCheck $ \c -> (not . hasEmptyElems) c ==> prop_num_delimters_cells_rows c

    it "checks a counterexample to the rule that #cells = #delimiters + #rows (for nonempty CSVs)" $ do
      --the first cell is stringified to an empty string, and the delimiter following it gets stripped 
      let csvAST1 = Csv [Row [Cell [Audio [Visible] 2 []],Cell [Bold "\RS5t\780762\106828&}F"]],Row [Cell [AudioInsert 3 [Italic "\NAK\ENQQw\1089214\&1\RS\177134",Italic "(p\290578\1093187",Cloze 5 "\">",Cloze 5 "M\35977\822837\NUL"],AudioInsert 6 [AudioInsert 5 [AudioInsert 5 [Italic "",Regular "!|V\414566"],Regular "\SIfm\"",Cloze 8 "",Bold "\600406\DC2b\ENQBjI(p",Cloze 8 "\893517"],AudioInsert 3 [Bold "=;\746383iv\r",Cloze 5 "\606896c\RS\303868",Cloze 5 "\EM",Regular "6\335771"],Italic "\ESCE\134090xe#\DC2",Bold "%\r"],Audio [Visible] 0 []],Cell [Italic "",Regular "\ETXl-\268442\&8\358202q",Audio [Invisible] 1 [],Italic "tD"]]]

      prop_num_delimters_cells_rows csvAST1 `shouldBe` True


  describe "csv: property testing" $ do

    it "numRowsAndCells" $ do
      let sample_csv = generate arbitrary :: IO CSV
      c <- sample_csv
      numRowsAndCells c `shouldBe` numRowsAndCells (updateIndices c)

    prop "updateIndices does not change how many rows and cells there are" $ 
      \c -> numRowsAndCells(updateIndices c) `shouldBe` numRowsAndCells c

    -- control the number of tests in case too slow:
    -- modifyMaxSize (const 110) $ it "generates marked up text files and parses them" $ property $
    prop "rewriteIndices does not change how many rows and cells there are" $
      \c -> numRowsAndCells(rewriteIndices c) `shouldBe` numRowsAndCells c

    -- control the number of sample tests with:
    -- modifyMaxSize (const 110) $ it "generates marked up text files and parses them" $ property $
    it "generates marked up text files and parses them" $ do
      let prop_succesful_parse c = parse csv "" `shouldSucceedOn` (stringifyBack c)

      quickCheck $ withMaxSuccess 10 prop_succesful_parse

    prop "for nonempty csvs, #cells = #delimiters + #rows" $
      \c -> (not . hasEmptyElems) c ==>
              prop_num_delimters_cells_rows c


  describe "csv parsers: expected failures" $ do

    it "cell and row failures" $ do

      -- future tests should allow embedding of markers, but for now our parsers fail on:
      let embedding1 = "*foo _bla_ * @audio @embedded *audio*@ rest@ or @_audio_ @embedded@@ or @@embedded@@"
      let embedding2 = "@*_bold italic_* and ~_hidden _audio_ string_~@\n---\nyour sound file: |>\n---\n*_good_ bye*"

      parse cell "" `shouldFailOn` embedding1
      parse row "" embedding2 `shouldParse` Row []

    it "checks that blocks flanked by delimiters cannot be parsed" $ do

        -- three blocks flanked by delimiters: [flanked left, flanked right, flanked left & right]
      let blocks_flanked =
            let del = "\n---\n"
                clozeStr = " blah ~foo~1 blah ~bar~1 " 
                audioStr = "I want @this determiner@1 converted to sound"
                insertStr = "1|> (insert to the left)"
                blahStr = " blah *blah* _blah_ blah, boring "
                coreStr = clozeStr ++ del ++ insertStr ++ del ++ blahStr ++ del ++ audioStr
            in [ del ++ coreStr
               , coreStr ++ del
               , del ++ coreStr ++ del
               ] 

      parse csv "" `shouldFailOn` (blocks_flanked !! 0) -- delimiter to the left
      parse csv "" `shouldFailOn` (blocks_flanked !! 1) -- delimiter to the right
      parse csv "" `shouldFailOn` (blocks_flanked !! 2) -- delimiter on both sides

    -- its only use is to collect corner cases for the next test
    it "collects random strings that may generate failure (used for info)" $ do
      csv_strs <- sample' (resize 1000 $ vectorOf 5 arbitraryASCIIChar) :: IO [String]
      let checkSuccessOn [] = putStrLn $ "+++ OK, passed " ++ show (length csv_strs) ++ " tests"
          checkSuccessOn (str:strs) =
            let parserOf s = parse (csvWith $ rowWith id) "" s
            in if isLeft (parserOf str) then putStrLn str else checkSuccessOn strs 
      checkSuccessOn csv_strs

    it "some expected failures on random strings" $ do
      parse csv "" `shouldFailOn` "\",8_<" -- unmatched italic marker _
      parse csv "" `shouldFailOn` ",*r<" -- unmatched bold marker *
      parse csv "" `shouldFailOn` ",~}" -- unmatched audio marker ~

    it "checks that csv parser fails on random strings" $ do
      csv_strs <- sample' (resize 100 arbitrary) :: IO [String]
      let parses =  parse (csvWith $ rowWith id) "" <$> csv_strs
      any isLeft parses `shouldBe` True

    it "checks that empty cells cannot be parsed" $ do
      parse csv "" `shouldFailOn` "\n--\nfoo\n--\n\n--\nbar"
      parse csv "" `shouldFailOn` "foo\n--\n\n--\nbar"

-- property used in a couple of tests above
prop_num_delimters_cells_rows c = numDelimiters == numCells - numRows
  where
    numDelimiters = T.count "\n---\n" (T.pack $ stringifyBack c)
    numRows = fst $ numRowsAndCells c
    numCells = snd $ numRowsAndCells c



