{-# LANGUAGE NoImplicitPrelude #-}
module AudioSpec (spec) where

import Prelude (repeat,concat,head,(!!))
import Data.List (nub)
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Import hiding (many,some,try)
import Test.Hspec
import Test.QuickCheck


import Audio
import Parser
import Generators()
import Util (isEmptyInsert,getIndex,isDefaultMarkup,unwrapCSV,getAudios,isAudio,flattenAudios,getIndices)
import Write (ankiStringifyCSV) -- delete after finishing testing csv


spec :: Spec
spec = do


  describe "audio" $ do

    it "parses audio strings, i.e. strings running between audio markers" $ do
      let gibberish = "#[\'foo \nbar[<blah|]\']&\n(\"foo-bar!\")"
      let gibberish_ws = " #[\'foo \nbar[<blah|]\']&\n(\"foo-bar!\") "
      let pm = parse (many markup) ""
      let Right ms = pm gibberish
      let Right ms_ws = pm gibberish_ws
      let pc = parse cell ""
      let Right c1 = pc $ "@" ++ gibberish ++ "@1"
      let Right c2 = pc $ "@ " ++ gibberish ++ " @"
      let Right c3 = pc $ "@" ++ gibberish ++ "@2"
      let Right c4 = pc $ "-@" ++ gibberish ++ "@"
      let Right c5 = pc $ "@" ++ gibberish ++ "@666"
      let Right c6 = pc $ "--@" ++ gibberish ++ "@5"

      head (getAudios c1) `shouldBe` (Audio [Visible] 1 ms)
      head (getAudios c2) `shouldBe` (Audio [Visible] 0 ms_ws)
      head (getAudios c3) `shouldBe` (Audio [Visible] 2 ms)
      head (getAudios c4) `shouldBe` (Audio [Invisible] 0 ms)
      head (getAudios c5) `shouldBe` (Audio [Visible] 666 ms)
      head (getAudios c6) `shouldBe` (Audio [Invisible] 5 ms)


  describe "placeAudio" $ do


    it "uses placeAudio to copy markups from an Audio element to an AudioInsert element" $ do

      let myaudio = Audio [Visible] 7 [Regular "AUDIO STR"]
      let mrks = [Regular "INSERT HERE>>>> ", AudioInsert 7 []]
      let mrks_res = [Regular "INSERT HERE>>>> ", AudioInsert 7 [Regular "AUDIO STR"]]

      placeAudio myaudio mrks `shouldBe` mrks_res


    it "tests placeAudio to see if audio contents (markups) are copied correctly" $ do

      let myaudio = Audio [Visible] 5 [Regular "audio ", Cloze 3 "str", Regular "ing"]
      let cells = [ Audio [Visible] 666 [ Regular "audio which does "
                                        , Bold "not", Regular " get copied"]
                  , Regular " "
                  , AudioInsert 5 []
                  , Bold "###2nd default INSERT"
                  , Regular " "
                  , Italic "ciao"
                  , Regular "!"
                  ]
      let cells_res = [ Audio [Visible] 666 [ Regular "audio which does "
                                            , Bold "not", Regular " get copied"]
                      , Regular " "
                      , AudioInsert 5 [Regular "audio ", Cloze 3 "str", Regular "ing"]
                      , Bold "###2nd default INSERT"
                      , Regular " "
                      , Italic "ciao"
                      , Regular "!"
                      ]

      placeAudio myaudio cells `shouldBe` cells_res


    it "tests copyAudios to see if audio contents (markups) are copied correctly" $ do

      let myaudios = [ Audio [Visible] 5 [Regular "xxxxx ", Cloze 3 "xxx", Regular "xxx"]
                     , Audio [Visible] 7 [Regular "ooooo ", Cloze 3 "ooo", Regular "ooo"]
                     ]
      let cells = [ Audio [Visible] 666 [ Regular "audio which does "
                                        , Bold "not", Regular " get copied"]
                  , Regular " "
                  , AudioInsert 5 []
                  , Bold "###2nd default INSERT"
                  , Regular " "
                  , AudioInsert 7 []
                  , Italic "ciao"
                  , Audio [Visible] 7 [Regular "ooooo ", Cloze 3 "ooo", Regular "ooo"]
                  , Regular "!"
                  , Audio [Visible] 5 [Regular "xxxxx ", Cloze 3 "xxx", Regular "xxx"]
                  ]
      let cells_res = [ Audio [Visible] 666 [ Regular "audio which does "
                                            , Bold "not", Regular " get copied"]
                      , Regular " "
                      , AudioInsert 5 [Regular "xxxxx ", Cloze 3 "xxx", Regular "xxx"]
                      , Bold "###2nd default INSERT"
                      , Regular " "
                      , AudioInsert 7 [Regular "ooooo ", Cloze 3 "ooo", Regular "ooo"]
                      , Italic "ciao"
                      , Audio [Visible] 7 [Regular "ooooo ",Cloze 3 "ooo",Regular "ooo"]
                      , Regular "!"
                      , Audio [Visible] 5 [Regular "xxxxx ",Cloze 3 "xxx",Regular "xxx"]
                      ]

      copyAudios (Cell cells) `shouldBe` (Cell cells_res)


    it "replaces first insertion marker with an audio element and deletes the remaining markers" $ do
      let cell1 = "blah  @something\n or other@ *blah*\n foo bar = foo\n bar"
      let cell2 = "a string *without* insertion, _please_"
      let cell3 = "~insert it~ |>again _please_|>"
      let nls = "\n\n\n"
      let del = "\n---\n"
      let row1 = cell1 ++ del ++ cell2 ++ del ++ cell3 ++ nls
      let csv1 = concat $ take 2 $ repeat row1

      -- cells with audio insertions (except cell2_res)
      let cell1_res = Cell [ Regular "blah  "
                           , Audio [Visible] 0 [Regular "something\n or other"]
                           , Regular " "
                           , Bold "blah"
                           , Regular "\n foo bar = foo\n bar"
                           ]

      let cell2_res = Cell [ Regular "a string "
                           , Bold "without"
                           , Regular " insertion, "
                           , Italic "please"
                           ]

      let cell3_res = Cell [ Cloze 0 "insert it"
                           , Regular " "
                           , AudioInsert 0 [Regular "audio string"]
                           , Regular "again "
                           , Italic "please"
                           ]

      -- cell3_res without audio insertions
      let cell3_res' = Cell [ Cloze 0 "insert it"
                           , Regular " "
                           , AudioInsert 0 [Regular "something\n or other"]
                           , Regular "again "
                           , Italic "please"
                           ]


      let row1_res = Row [cell1_res, cell2_res, cell3_res']
      let csv1_res = Csv $ take 2 $ repeat row1_res

      let audio = Audio [Visible] 0 [Regular "audio string"]

      -- placeAudioInCell :: Markup -> Cell -> Cell
      let placeAudioInCell m (Cell ms) = Cell $ placeAudio m ms
          placeAudioInCell _ noncell = noncell

      parse (placeAudioInCell audio <$> cell) "" cell1 `shouldParse` cell1_res
      parse (placeAudioInCell audio <$> cell) "" cell2 `shouldParse` cell2_res
      parse (placeAudioInCell audio <$> cell) "" cell3 `shouldParse` cell3_res
      parse (placeAudioInCell audio <$> rowIgnoringIndices) "" row1 `shouldParse` row1_res
      parse (placeAudioInCell audio <$> csvIgnoringIndices) "" csv1 `shouldParse` csv1_res


    it "parses audio string markers among other elements of Markup" $ do

      let cell1 = "this ~blah~ ups <| *ups*" :: String
      let del = "\n---\n"

      let rowStr = "11111 " ++ cell1 ++ del ++ "222222 " ++ cell1 ++ del ++ "333333 " ++ cell1 ++ "\n\n"
      let multiRowStr = concat $ take 5 $ repeat rowStr

      parse row "" `shouldSucceedOn` rowStr
      parse csv "" `shouldSucceedOn` multiRowStr


      let cell2 = "this ~1st~1 |> and/or <| _ups_ ~2nd~1 foo--bar ~3rd blah~999 'ups'?!\n\n" :: String

      parse cell "" `shouldSucceedOn` cell2


    it "shows that clozes and audio strings can overlap in very restricted ways" $ do

      let cell5 = "this ~1st~ @ or$%^ _ups_ @5 ~2nd~1 and-that ~3rd blah~ ups *ups*!\n---\n" :: String
      let cell6 = "this ~1st~1 and/or _ups_ ~2nd~1 foo--bar ~3rd @ blah~999 'ups'? * *!@7\n\n" :: String

      let txtRow = cell5 ++ cell5 ++ cell6
      let txtRows = concat $ take 5 $ repeat txtRow

      parse cell "" `shouldSucceedOn` cell5
      --this should fail, because audio marker occurs in cloze string
      parse cell "" `shouldFailOn` cell6
      -- these should fail as well
      parse csv "" `shouldFailOn` txtRows
      parse csv "" `shouldFailOn` txtRows

{-NB: Only one kind of overlap/overlay between audio and cloze is permissible.

Partial overlaps between audio and cloze is not allowed:

@audio_word ~audio and cloze phrase@ cloze phrase~

Nor is a wrap of cloze around audio allowed:

~ cloze phrase 1 @audio and cloze phrase@ cloze phrase 2~

But wrapping audio markers around cloze markers is allowed:

@audio word 1 ~audio and cloze phrase~ audio phrase 2@

NB: audio strings occur within field/cell boundaries; so audio markers cannot cross these boundaries

-}


  describe "getAudios" $ do

    it "gets audio elements from cells, rows, and csv elements" $ do

      let cell1 = "@ a ~few words~,\n some *bold*,\n some _stressed_ but all audio @ yes!"
      let cell2 = "a few words\n with one\n @audio string@1"
      let nl = "\nl"
      let nls = "\n\n\n"
      let del = "\n--\n"

      let row1 = cell1 ++ del ++ cell2 ++ nl
      let row2 = cell2 ++ del ++ cell1 ++ nl

      let csv1 = row1 ++ nls ++ row2 ++ nls ++ row1
      let csv2 = row2 ++ nls ++ row1 ++ nls ++ row2 ++ nls

      let Right pcell1_res = parse cell "" cell1
      let Right pcell2_res = parse cell "" cell2

      let Right prow1_res = parse row "" row1
      let Right prow2_res = parse row "" row2

      let Right pcsv1_res = parse csv "" csv1
      let Right pcsv2_res = parse csv "" csv2

      let Cell ms = pcell1_res in (any isAudio ms) `shouldBe` True
      let Cell ms = pcell2_res in (any isAudio ms) `shouldBe` True


    it "parses another multi-row csv" $ do
      let cell1 = "blah |> *blah*\n foo bar |> = foo\n bar"
      let cell2 = "a string *without* insertion, _please_"
      let cell3 = "~insert it~ |> again |> _please_|>"
      let nls = "\n\n\n"
      let del = "\n---\n"
      let row1 = cell1 ++ del ++ cell2 ++ del ++ cell3 ++ nls
      let csv1 = concat $ take 2 $ repeat row1

      -- cells with audio insertions (except cell2_res)
      let cell1_res = Cell [ Regular "blah "
                           , AudioInsert 0 []
                           , Regular " "
                           , Bold "blah"
                           , Regular "\n foo bar "
                           , AudioInsert 0 []
                           , Regular " = foo\n bar"
                           ]

      let cell2_res = Cell [ Regular "a string "
                           , Bold "without"
                           , Regular " insertion, "
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

      let row1_res = Row [cell1_res, cell2_res, cell3_res]
      let csv1_res = Csv $ take 2 $ repeat row1_res

      let audio = Audio [Visible] 0 [Regular "audio string"]

      parse csvIgnoringIndices "" csv1 `shouldParse` csv1_res

  describe "mergeCoindexedAudios" $ do

    it "checks the merge of two adjacent co-indexed audio elements" $ do
      let audios = [Audio [] 33 [Regular "this one "], Audio [] 33 [Regular "and the next need merging"]]
      let mergedAudios = [Audio [] 33 [Regular "this one ",Regular "and the next need merging"]]
      mergeCoindexedAudios audios `shouldBe` mergedAudios

    it "checks the merge of adjacent co-indexed audio elements" $ do
      let audios = [Audio [] 2 [Italic "and that"], Audio [] 1 [Regular "this "], Audio [] 1 [Bold "one"]]
      let mergedAudios = [Audio [] 1 [Regular "this ", Bold "one"], Audio [] 2 [Italic "and that"]]
      mergeCoindexedAudios audios `shouldBe` mergedAudios

  describe "placeAudioManyTimesWith" $ do

    it "inserts audio markups n times" $ do

        let aud = Audio [] 3 [Italic "I", Bold "really", Italic "crave to be moved!!!!!"]

        let marks = [ Regular "one"
                    , Regular "two"
                    , Audio [] 55 [Bold "audio content"]
                    , Audio [] 66 []
                    , Regular "just the beginning"
                    , AudioInsert 3 []
                    , AudioInsert 3 []
                    , AudioInsert 3 []
                    , Bold "three"
                    , Audio [] 7 [Bold "don't touch"]
                    , Bold "four"
                    , AudioInsert 11 []
                    , AudioInsert 11 []
                    , AudioInsert 11 []
                    , AudioInsert 11 []
                    , Bold "five"
                    , Audio [] 11 [Bold "move me please"]
                    ]


        let emptyAud3 ms = filter (\m -> isEmptyInsert m && getIndex m == Just 3) ms

        let ms1 = placeAudioManyTimesWith 1 (\_ -> True) aud marks
        length (emptyAud3 ms1) `shouldBe` 2

        let ms2 = placeAudioManyTimesWith 2 (\_ -> True) aud marks
        length (emptyAud3 ms2) `shouldBe` 1

        let ms3 = placeAudioManyTimesWith 3 (\_ -> True) aud marks
        length (emptyAud3 ms3) `shouldBe` 0

        let ms8 = placeAudioManyTimesWith 8 (\_ -> True) aud marks
        length (emptyAud3 ms8) `shouldBe` 0

    it "inserts three sets of audio markups at three different insert places" $ do

        let aud1 = Audio [] 0 [Italic "ONE ONE ONE!!!!!"]
        let aud2 = Audio [] 0 [Italic "TWO TWO TWO!!!!!"]
        let aud3 = Audio [] 0 [Italic "THREE THREE THREE!!!!!"]

        let mks = [ Regular "one"
                  , Regular "two"
                  , Audio [] 55 [Bold "audio content"]
                  , Audio [] 66 []
                  , Regular "just the beginning"
                  , AudioInsert 0 []
                  , AudioInsert 0 []
                  , AudioInsert 0 []
                  , Bold "three"
                  , Audio [] 7 [Bold "don't touch"]
                  , Bold "four"
                  , AudioInsert 11 []
                  , AudioInsert 11 []
                  , AudioInsert 11 []
                  , AudioInsert 11 []
                  , Bold "five"
                  , Audio [] 11 [Bold "move me please"]
                  ]

        let res = [ Regular "one"
                  , Regular "two"
                  , Audio [] 55 [Bold "audio content"]
                  , Audio [] 66 []
                  , Regular "just the beginning"
                  , AudioInsert 0 [Italic "ONE ONE ONE!!!!!"]
                  , AudioInsert 0 [Italic "TWO TWO TWO!!!!!"]
                  , AudioInsert 0 [Italic "THREE THREE THREE!!!!!"]
                  , Bold "three"
                  , Audio [] 7 [Bold "don't touch"]
                  , Bold "four"
                  , AudioInsert 11 []
                  , AudioInsert 11 []
                  , AudioInsert 11 []
                  , AudioInsert 11 []
                  , Bold "five"
                  , Audio [] 11 [Bold "move me please"]
                  ]


        let myres =
              let putOnce x ms = placeAudioManyTimesWith 1 isDefaultMarkup x ms
              in putOnce aud3 . putOnce aud2 . putOnce aud1 $ mks

        myres `shouldBe` res

  describe "copyAudios & friends" $ do

    it "checks the isDefaultMarkup restriction on copying audios markups is working" $ do

      let myauds = [Audio [] 1 [Regular "ONE"], Audio [] 0 [Regular "TWO"]]
      let mins = [AudioInsert 1 [], AudioInsert 0 []]

      let res_def = placeMultipleAudiosOnceWith isDefaultMarkup myauds mins
      let res_ctm = placeMultipleAudiosOnceWith (not .isDefaultMarkup) myauds mins

      res_def `shouldBe` [AudioInsert 1 [], AudioInsert 0 [Regular "TWO"]]
      res_ctm `shouldBe` [AudioInsert 1 [Regular "ONE"], AudioInsert 0 []]

    it "places two audios at two different (but co-indexed) insert places" $ do

      let mymin = Csv [Row [ Cell [ Bold "bold", AudioInsert 0 [] ]
                           , Cell [ AudioInsert 0 [], Italic "italic"] ]
                           ]
      let myauds = [Audio [] 0 [Regular "ONE"], Audio [] 0 [Regular "TWO"]]

      let myres = [ Bold "bold"
                  , AudioInsert 0 [Regular "ONE"]
                  , AudioInsert 0 [Regular "TWO"]
                  , Italic "italic"
                  ]

      placeMultipleAudiosOnceWith isDefaultMarkup myauds (unwrapCSV mymin) `shouldBe` myres


    it "copyAudios: inserts merged default (zero-indexed) audio markups at every co-indexed insert place" $ do

      copyAudios mygen `shouldBe` res1


    it "placeMultipleAudiosOnceInListsWith: insert audio contents once in lists of lists" $ do

      let as = [Audio [] 0 [Bold "ONE"],Audio [] 0 [Bold "TWO"]]

      let mymss = [ [Audio [] 0 [Bold "ONE"], Audio [] 1 [Italic ""]]
                  , [Audio [] 3 [Bold "no"]]
                  , [Italic "", AudioInsert 1 []]
                  , [AudioInsert 0 [], Bold "hmm"]
                  , [AudioInsert 0 []]
                  , [Audio [] 0 [Bold "TWO"]]
                  ]

      let myres = [ [Audio [] 0 [Bold "ONE"], Audio [] 1 [Italic ""]]
                  , [Audio [] 3 [Bold "no"]]
                  , [Italic "", AudioInsert 1 []]
                  , [AudioInsert 0 [Bold "ONE"], Bold "hmm"]
                  , [AudioInsert 0 [Bold "TWO"]]
                  , [Audio [] 0 [Bold "TWO"]]
                  ]

      placeMultipleAudiosOnceInListsWith isDefaultMarkup as mymss `shouldBe` myres


    it "copyUnmergedAudiosWith: inserts merged default (zero-indexed) audio markups at different default insert places" $ do

      let myrow = Row [ Cell [Audio [] 0 [Bold "ONE"], Audio [] 1 [Italic ""]]
                      , Cell [Audio [] 3 [Bold "no"]]
                      , Cell [Italic "", AudioInsert 1 []]
                      , Cell [AudioInsert 0 [], Bold "hmm"]
                      , Cell [Italic "here: ", AudioInsert 0 []]
                      , Cell [Audio [] 0 [Bold "TWO"]]
                      ]

      let myres = Row [ Cell [Audio [] 0 [Bold "ONE"],Audio [] 1 [Italic ""]]
                      , Cell [Audio [] 3 [Bold "no"]]
                      , Cell [Italic "",AudioInsert 1 []]
                      , Cell [AudioInsert 0 [Bold "ONE"],Bold "hmm"]
                      , Cell [Italic "here: ", AudioInsert 0 [Bold "TWO"]]
                      , Cell [Audio [] 0 [Bold "TWO"]]
                      ]

      copyUnmergedAudiosWith myrow isDefaultMarkup `shouldBe` myres


    it "copyUnmergedAudiosWith: makes up default audio-insert pairs and moves markups within each pair" $ do

      let csv_orig = Csv [Row [Cell [ Audio [] 0 [Bold "one"]
                                    , AudioInsert 0 []
                                    , Audio [] 0 [Bold "two"]
                                    , AudioInsert 0 []
                                    , AudioInsert 0 []
                                    , AudioInsert 0 []
                                    , Audio [] 0 [Bold "three"]
                                    , Audio [] 0 [Bold "four"]
                                    ] 
                              ]
                         ]

      let csv_move = Csv [Row [Cell [ Audio [] 0 [Bold "one"]
                                    , AudioInsert 0 [Bold "one"]
                                    , Audio [] 0 [Bold "two"]
                                    , AudioInsert 0 [Bold "two"]
                                    , AudioInsert 0 [Bold "three"]
                                    , AudioInsert 0 [Bold "four"]
                                    , Audio [] 0 [Bold "three"]
                                    , Audio [] 0 [Bold "four"]
                                    ] 
                              ]
                         ]

      copyUnmergedAudiosWith csv_orig (\_ -> True) `shouldBe` csv_move


    it "copyUnmergedAudios: check that it does not update indices" $ do

      let oldInds c = nub $ getIndices c
      let newInds c = nub $ getIndices $ copyUnmergedAudios c
      let prop_no_index_update c = oldInds c == newInds c
      quickCheck prop_no_index_update

    it "copyAudios increases (or at least leaves the same) the markup count of a CSV element" $ do

      let countMarkups c = length $ flattenAudios c 
      let prop_copy_increase c = countMarkups (copyAudios c) >= countMarkups c
      quickCheck prop_copy_increase

    it "copyUnmergedAudiosWith: inserts merged default (zero-indexed) audio markups at different insert places" $ do

      -- wrong: the markup contents of the first default Audio is copied in *every* insert place!
      copyUnmergedAudiosWith mygen isDefaultMarkup `shouldBe` res2


    it "copyUnmergedAudios: insert default audio markups at different default insert places" $ do

      let multi1 = "here are @a few words@; they @can be pronounced@ and be @pronounced later@\n--\ninsert here: |>\n--\nthe next here: |>; and\nthe last here: |>. Thanks!"

      let pmulti1 = ankiStringifyCSV . copyUnmergedAudios <$> parse (csvWith $ rowWith id) "" multi1

      let multi1_res = Right "\"here are a few words; they can be pronounced and be pronounced later\",\"insert here: [sound:a_few_words.mp3]\",\"the next here: [sound:can_be_pronounced.mp3]; and<br>the last here: [sound:pronounced_later.mp3]. Thanks!\""

      pmulti1 `shouldBe` multi1_res


    it "placeMultipleAudiosWith: inserts merged default (zero-indexed) audio markups at different insert places" $ do

      -- markup contents are copied in the first AudioInsert and the other co-indexed inserts are removed
      placeMultipleAudiosWith isDefaultMarkup auds (unwrapCSV mygen) `shouldBe` res3

    it "placeAudioOnceWith: inserts merged default audio markups at different default insert places" $ do

      let incrementalPlacements = placeAudioOnceWith isDefaultMarkup (auds !! 1) $
            placeAudioOnceWith isDefaultMarkup (auds !! 0) (unwrapCSV mygen)

      incrementalPlacements `shouldBe` res4


auds = [Audio [] 0 [Regular "USE ME!!!!!!!!!!!!"], Audio [] 0 [Regular "DON'T LEAVE ME!!!!!!!!!!!"]]

mygen = Csv [Row [ Cell [ Regular "cell start"
                        , Audio [] 0 [Cloze 5 "", Cloze 24 ""]
                        , Cloze 0 ""
                        , Regular ""
                        , AudioInsert 0 []
                        , Cloze 6 ""
                        , Audio [] 7 [ Regular "", Cloze 0 ""]
                        , Bold ""
                        ]
                 , Cell [ Regular "",Italic "",Cloze 0 "",Bold "", AudioInsert 7 [], AudioInsert 0 []]
                 , Cell [ Regular "",Audio [] 0 [Cloze 7 "",Cloze 9 ""], AudioInsert 0 []]
                 , Cell [ Cloze 0 "",AudioInsert 3 [],Audio [] 0 [Cloze 0 ""]]
                 , Cell [ Regular "",Audio [] 3 [Regular "audio",Italic "string"],Cloze 0 ""]
                 ]
            ]

-- the markups contents of *all* default Audios are copied at *every* insert place!
res1 = Csv [Row [Cell [Regular "cell start",Audio [] 0 [Cloze 5 "",Cloze 24 ""],Cloze 0 "",Regular "",AudioInsert 0 [Cloze 5 "",Cloze 24 "",Cloze 7 "",Cloze 9 "",Cloze 0 ""],Cloze 6 "",Audio [] 7 [Regular "",Cloze 0 ""],Bold ""],Cell [Regular "",Italic "",Cloze 0 "",Bold "",AudioInsert 7 [Regular "",Cloze 0 ""],AudioInsert 0 [Cloze 5 "",Cloze 24 "",Cloze 7 "",Cloze 9 "",Cloze 0 ""]],Cell [Regular "",Audio [] 0 [Cloze 7 "",Cloze 9 ""],AudioInsert 0 [Cloze 5 "",Cloze 24 "",Cloze 7 "",Cloze 9 "",Cloze 0 ""]],Cell [Cloze 0 "",AudioInsert 3 [Regular "audio",Italic "string"],Audio [] 0 [Cloze 0 ""]],Cell [Regular "",Audio [] 3 [Regular "audio",Italic "string"],Cloze 0 ""]]]

res2 = Csv [Row [ Cell [ Regular "cell start"
                       , Audio [] 0 [Cloze 5 "",Cloze 24 ""]
                       , Cloze 0 ""
                       , Regular ""
                       , AudioInsert 0 [Cloze 5 "",Cloze 24 ""]
                       , Cloze 6 ""
                       , Audio [] 7 [Regular "",Cloze 0 ""]
                       , Bold ""
                       ]
                , Cell [ Regular ""
                       , Italic ""
                       , Cloze 0 ""
                       , Bold ""
                       , AudioInsert 7 []
                       , AudioInsert 0 [Cloze 7 "",Cloze 9 ""]
                       ]
                , Cell [ Regular ""
                       , Audio [] 0 [Cloze 7 "",Cloze 9 ""]
                       , AudioInsert 0 [Cloze 0 ""]]
                       , Cell [Cloze 0 ""
                       , AudioInsert 3 []
                       , Audio [] 0 [Cloze 0 ""]
                       ]
                , Cell [ Regular ""
                       , Audio [] 3 [Regular "audio",Italic "string"]
                       , Cloze 0 ""
                       ]
                ]
           ]

res3 = [Regular "cell start",Audio [] 0 [Cloze 5 "",Cloze 24 ""],Cloze 0 "",Regular "",AudioInsert 0 [Regular "USE ME!!!!!!!!!!!!"],Cloze 6 "",Audio [] 7 [Regular "",Cloze 0 ""],Bold "",Regular "",Italic "",Cloze 0 "",Bold "",AudioInsert 7 [],Regular "",Audio [] 0 [Cloze 7 "",Cloze 9 ""],Cloze 0 "",AudioInsert 3 [],Audio [] 0 [Cloze 0 ""],Regular "",Audio [] 3 [Regular "audio",Italic "string"],Cloze 0 ""]

res4 = [Regular "cell start",Audio [] 0 [Cloze 5 "",Cloze 24 ""],Cloze 0 "",Regular "",AudioInsert 0 [Regular "USE ME!!!!!!!!!!!!"],Cloze 6 "",Audio [] 7 [Regular "",Cloze 0 ""],Bold "",Regular "",Italic "",Cloze 0 "",Bold "",AudioInsert 7 [],AudioInsert 0 [Regular "DON'T LEAVE ME!!!!!!!!!!!"],Regular "",Audio [] 0 [Cloze 7 "",Cloze 9 ""],AudioInsert 0 [],Cloze 0 "",AudioInsert 3 [],Audio [] 0 [Cloze 0 ""],Regular "",Audio [] 3 [Regular "audio",Italic "string"],Cloze 0 ""]

