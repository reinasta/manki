{-# LANGUAGE NoImplicitPrelude #-}
module IndicesSpec (spec) where

import Prelude (repeat,concat,head,maximum)
import Data.List (sort,group)
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Import hiding (many,some,try)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Indices
import Util (isDefaultCloze,isDefaultInsert,isDefaultAudio,getIndex,isCloze,isInsert,
            flattenAudios,isAudio,getIndicesWith,bareBones,getAudios,getInserts,getClozes,getRows)
import Schema (genFromSchemaCSV,getSchemaString)
import Types
import Generators
import Parser


spec :: Spec
spec = do
  describe "updateClozeIndices" $ do

    it "checks that all the default (zero-indexed) clozes are updated with new, non-zero indices" $ do
      let row0 = Row [Cell [Regular "this ",Cloze 0 "non-indexed",Regular " string. One ",Cloze 6 "indexed",Regular ". And ",Cloze 0 "another non-indexed",Regular " finally"],Cell [Regular "this ",Cloze 0 "non-indexed",Regular " string. One ",Cloze 6 "indexed",Regular ". And ",Cloze 0 "another non-indexed",Regular " finally"],Cell [Cloze 0 "without index",Regular " and ",Cloze 9 "with nine",Regular " and ",Cloze 0 "without",Regular " full stop. "],Cell [Regular "this ",Cloze 0 "non-indexed",Regular " string. One ",Cloze 6 "indexed",Regular ". And ",Cloze 0 "another non-indexed",Regular " finally"],Cell [Regular "this ",Cloze 0 "non-indexed",Regular " string. One ",Cloze 6 "indexed",Regular ". And ",Cloze 0 "another non-indexed",Regular " finally"],Cell [Cloze 0 "without index",Regular " and ",Cloze 9 "with nine",Regular " and ",Cloze 0 "without",Regular " full stop. "],Cell [Regular "this ",Cloze 0 "non-indexed",Regular " string. One ",Cloze 6 "indexed",Regular ". And ",Cloze 0 "another non-indexed",Regular " finally"],Cell [Regular "this ",Cloze 0 "non-indexed",Regular " string. One ",Cloze 6 "indexed",Regular ". And ",Cloze 0 "another non-indexed",Regular " finally"],Cell [Cloze 0 "without index",Regular " and ",Cloze 9 "with nine",Regular " and ",Cloze 0 "without",Regular " full stop. "]]
      let row0_updated = updateClozeIndices row0
      length (filter isDefaultCloze $ flattenAudios row0_updated) `shouldBe` 0


    it "checks that all the default (zero-indexed) audio-inserts are updated with non-zero indices" $ do
      let icsv0 = Csv [Row [Cell [AudioInsert 0 [], Audio [] 75 [AudioInsert 0 [], AudioInsert 0 []], AudioInsert 0 [], AudioInsert 0 [],AudioInsert 0 [], AudioInsert 0 []], Cell [AudioInsert 0 [], AudioInsert 888 [], AudioInsert 0 [], Regular "this ",Cloze 0 "1st",Regular " ",Audio [] 0 [AudioInsert 0 [], AudioInsert 0 [], AudioInsert 0 [], Regular "or$%^ ",Italic "ups"],Regular " ",Cloze 1 "2nd",Regular " ",AudioInsert 5 [],Regular "and-that ",Cloze 0 "3rd blah",Regular " ",Audio [] 0 [Regular "ups ",Bold "ups",Regular "!"]],Cell [Regular "this ",Cloze 0 "1st",Regular " ",Audio [] 5 [Regular "or$%^ ",Italic "ups"],Regular " ",Cloze 1 "2nd",Regular " ",AudioInsert 999 [],Regular "and-that ",Cloze 0 "3rd blah",Regular " ",Audio [] 5 [Regular "ups ",Bold "ups",Regular "!"]],Cell [Audio [] 2 [Regular "this"],Regular " ",Cloze 1 "1st",Regular " ",Audio [] 7 [Regular "and/or ",Italic "ups"],Regular " ",Cloze 1 "2nd",Regular " ",Audio [] 1 [ AudioInsert 0 [], AudioInsert 555 [], AudioInsert 0 [], AudioInsert 0 [], AudioInsert 0 [], Regular "this"],Regular " foo--bar ",Cloze 0 "3rd",Regular " ",AudioInsert 0 [],Regular "99 'ups' ", AudioInsert 0 [], Bold "ha",Regular "!"]]]
      let icsv0_updated = updateInsertIndices icsv0
      length (filter isDefaultInsert $ flattenAudios icsv0_updated) `shouldBe` 0


    it "checks that all the default (zero-indexed) audio elements get non-zero indices" $ do
      let acsv0 = Csv [Row [Cell [Audio [] 0 [], Audio [] 0 []], Cell [Regular "this ",Cloze 0 "1st",Regular " ",Audio [] 0 [Regular "or$%^ ",Italic "ups"],Regular " ",Cloze 1 "2nd",Regular " ",AudioInsert 5 [],Regular "and-that ",Cloze 0 "3rd blah",Regular " ",Audio [] 0 [Regular "ups ",Bold "ups",Regular "!"]],Cell [Regular "this ",Cloze 0 "1st",Regular " ",Audio [] 5 [Regular "or$%^ ",Italic "ups"],Regular " ",Cloze 1 "2nd",Regular " ",AudioInsert 5 [],Regular "and-that ",Cloze 0 "3rd blah",Regular " ",Audio [] 5 [Regular "ups ",Bold "ups",Regular "!"]],Cell [Audio [] 2 [Regular "this"],Regular " ",Cloze 1 "1st",Regular " ",Audio [] 7 [Regular "and/or ",Italic "ups"],Regular " ",Cloze 1 "2nd",Regular " ",Audio [] 1 [Regular "this"],Regular " foo--bar ",Cloze 0 "3rd",Regular " ",AudioInsert 7 [],Regular "99 'ups' ",Bold "ha",Regular "!"]]]
      let acsv0_updated = updateAudioIndices acsv0
      length (filter isDefaultAudio $ flattenAudios acsv0_updated) `shouldBe` 0


    it "checks that no new duplicate indices (cross-references) are added on updating cloze indices" $ do

      let mycsv = Csv [ Row [ Cell [Cloze 7 "", Regular "", Cloze 34 "", AudioInsert 5 [], Bold ""]
                            , Cell [Regular "", Italic "", Cloze 67 "", Bold ""]
                            , Cell [Regular "", Audio [] 0 [Cloze 1 "", Cloze 9 ""]]
                            , Cell [Cloze 1 "", AudioInsert 3 [], Audio [] 0 [Cloze 8 ""]]
                            , Cell [Regular "", Cloze 7 ""]
                            ]
                      ]

      let myres = Csv [Row [Cell [Audio [] 0 [Cloze 5 "", Cloze 0 ""], Cloze 0 "",Regular "",Cloze 6 "",AudioInsert 5 [],Bold ""],Cell [Regular "",Italic "",Cloze 0 "",Bold ""],Cell [Regular "",Audio [] 0 [Cloze 7 "",Cloze 9 ""]],Cell [Cloze 0 "",AudioInsert 3 [],Audio [] 0 [Cloze 0 ""]],Cell [Regular "",Cloze 0 ""]]]

      let clozeCheck_mycsv =
            preserveDuplicates (myOlds isCloze $ updateClozeIndices mycsv) (myOlds isCloze $ mycsv)
      let clozeCheck_myres =
            preserveDuplicates (myOlds isCloze $ updateClozeIndices myres) (myOlds isCloze $ myres)

      clozeCheck_mycsv `shouldBe` True
      clozeCheck_myres `shouldBe` True

    it "checks that no new duplicate indices (cross-references) are added on updating audio indices" $ do

      let  csva = Csv [ Row [ Cell [Audio [] 7 [], Regular "", Audio [] 34 [], AudioInsert 5 [], Bold ""]
                            , Cell [Regular "", Italic "", Audio [] 67 [], Bold ""]
                            , Cell [Regular "", Audio [] 0 [Cloze 1 "", Cloze 9 ""]]
                            , Cell [Audio [] 1 [], AudioInsert 3 [], Audio [] 0 [Audio [] 8 []]]
                            , Cell [Regular "", Audio [] 7 [] ]
                            ]
                      ]

      let audioCheck_csva =
            preserveDuplicates (myOlds isAudio $ updateAudioIndices csva) (myOlds isAudio csva)


      audioCheck_csva `shouldBe` True


    it "checks that no new duplicate indices (cross-references) appear on updating insert indices" $ do

      let csvi =
            Csv [
               Row [ Cell [AudioInsert 7 [], Regular "", AudioInsert 34 [], AudioInsert 5 [], Bold ""]
                   , Cell [Regular "", Italic "", AudioInsert 67 [], Bold ""]
                   , Cell [Regular "", Audio [] 0 [AudioInsert 1 [], AudioInsert 9 []]]
                   , Cell [AudioInsert 1 [], AudioInsert 3 [], Audio [] 0 [AudioInsert 8 []]]
                   , Cell [Regular "", AudioInsert 7 []]
                   ]
                ]

      let insertCheck_csvi =
            preserveDuplicates (myOlds isInsert $ updateInsertIndices csvi) (myOlds isInsert csvi)

      insertCheck_csvi `shouldBe` True


    it "checks that cross-references between audio and insert elements are preserved" $ do

      let csv1 = Csv [Row [ Cell [Regular "warming up ..."]
                          , Cell [Audio [Visible] 5 [Bold "five"],Regular "break", AudioInsert 9 []]
                          , Cell [AudioInsert 5 [], Regular "|||", Audio [Visible] 9 [Bold "nine"]]
                          , Cell [Audio [Invisible] 12 [Bold "unmatched"], AudioInsert 13 [], Audio [] 21 []]
                          , Cell [Bold "raa", AudioInsert 33 [], Regular "hmm", Audio [] 22 [Bold "unmatched"]]
                          , Cell [Audio [Invisible] 44 [Italic "unmatched"], AudioInsert 66 [], Italic "bye" ]
                          ]
                     ]
      let csv1_updated = updateIndices csv1
      let csv1_rewritten = rewriteIndices csv1

      -- indices in csv1_updated
      let formerAudio5 = getIndexWithContent csv1_updated isAudio [Bold "five"]
      let formerInsert5 = getIndexWithContent csv1_updated isInsert [Bold "five"]
      let formerAudio9 = getIndexWithContent csv1_updated isAudio [Bold "nine"]
      let formerInsert9 = getIndexWithContent csv1_updated isInsert [Bold "nine"]
      -- indices in csv1_rewritten
      let formerAudio5' = getIndexWithContent csv1_rewritten isAudio [Bold "five"]
      let formerInsert5' = getIndexWithContent csv1_rewritten isInsert [Bold "five"]
      let formerAudio9' = getIndexWithContent csv1_rewritten isAudio [Bold "nine"]
      let formerInsert9' = getIndexWithContent csv1_rewritten isInsert [Bold "nine"]

      -- check that the audio and the insert that previously had index i still have the equal indices
      formerAudio5 `shouldBe` formerInsert5
      formerAudio9 `shouldBe` formerInsert9
      formerAudio5' `shouldBe` formerInsert5'
      formerAudio9' `shouldBe` formerInsert9'


    it "checks that cross-references between audio and insert elements are preserved" $ do

      let row_str1 = "@a@1, @b@2, @c@3\n--\n1|>2|>3|>blah\n--\n@d@4 blah 4|>, @zero@, |>"
      let row_str2 = "@a@11, @b@12, @c@13\n--\n11|>12|>13|>blah\n--\n@d@14 blah 14|>, @zero@, |>"
      let row_str3 = "13|>@f@7,@g@9,@h@11,@i@13\n--\n11|> blah\n--\n9|>blah7|> and finally 22|> 23|> @22@22 @23@23"
      let manki_str = row_str1 ++ row_str2 ++ row_str3

      -- original indices
      let insert_inds = (\c -> getIndicesWith c isInsert) <$> parse (csvWith $ rowWith id) "" manki_str
      let audio_inds = (\c -> getIndicesWith c isAudio) <$> parse (csvWith $ rowWith id) "" manki_str
      -- updated indices
      let insert_inds' = (\c -> getIndicesWith c isInsert) <$> parse csv "" manki_str
      let audio_inds' = (\c -> getIndicesWith c isAudio) <$> parse csv "" manki_str

      let inds = sort <$> liftM2 (++) insert_inds audio_inds
      let inds' = sort <$> liftM2 (++) insert_inds' audio_inds'

      --preserveDuplicates :: [Int] -> [Int] -> Bool
      liftM2 preserveDuplicates inds inds' `shouldBe` Right True


    it "checks that no new duplicate indices appear on updating indices" $ do

       let mygen = Csv [Row [ Cell [ Regular "cell start"
                                   , Audio [] 0 [Cloze 5 "", Cloze 0 ""]
                                   , Cloze 0 ""
                                   , Regular ""
                                   , Cloze 6 ""
                                   , Audio [] 7 [ Regular "", Cloze 0 ""]
                                   , Bold ""
                                   ]
                            , Cell [ Regular "",Italic "",Cloze 0 "",Bold "", AudioInsert 7 []]
                            , Cell [ Regular "",Audio [] 0 [Cloze 7 "",Cloze 9 ""]]
                            , Cell [ Cloze 0 "",AudioInsert 3 [],Audio [] 0 [Cloze 0 ""]]
                            , Cell [ Regular "",Audio [] 3 [Regular "audio",Italic "string"],Cloze 0 ""]
                            ]
                       ]

       let clozeCheck_mygen =
             preserveDuplicates (myOlds isCloze $ updateClozeIndices mygen) (myOlds isCloze mygen)
       -- 5,0,0,6,0,0,7,9,0,0,0
       -- 5,3,4,6,11,12,7,9,16,19,21

       let audioCheck_mygen =
             preserveDuplicates (myOlds isAudio $ updateAudioIndices mygen) (myOlds isAudio mygen)
       -- 0,7,0,0,3
       -- 1,7,5,8,3

       let insertCheck_mygen =
             preserveDuplicates (myOlds isInsert $ updateInsertIndices mygen) (myOlds isInsert mygen)
       -- 7,3
       -- 7,3

       clozeCheck_mygen `shouldBe` True
       audioCheck_mygen `shouldBe` True
       insertCheck_mygen `shouldBe` True

    it "rewriteIndices on csv2x3: checks that rewriting yields ascending indices without gaps" $ do
      csv2x3 <- genFromSchemaCSV 2 3 "A0 A3 A0 I0 I0 I3"
      --numRowsAndCells csv2x3 `shouldBe` (2,6)
      let csv2x3_str = getSchemaString $ rewriteIndices csv2x3
      csv2x3_str `shouldBe` "A1 A2 A1 I1 I1 I2"

    it "rewriteIndices on csv4x4: checks that rewriting yields ascending indices without gaps" $ do
      csv4x4 <- genFromSchemaCSV 4 4 "A0 A3 A0 I0 I0 I3 A4 I4 A5 I5 A7 A7 A8 I7 I8 I7"
      --numRowsAndCells csv2x3 `shouldBe` (2,6)
      let csv4x4_str = getSchemaString $ rewriteIndices csv4x4
      csv4x4_str `shouldBe` "A1 A2 A1 I1 I1 I2 A3 I3 A4 I4 A5 A5 A6 I5 I6 I5"

    it "rewriteIndices: a problem case" $ do

      let r = Row [Cell [],Cell [Audio [Invisible] 15 [Audio [Visible] 22 [Audio [Invisible] 19 []],Audio [Visible] 22 [Audio [Invisible] 16 [],Audio [Invisible] 22 []],Audio [Invisible] 16 [Audio [Invisible] 2 [],Audio [Invisible] 5 []],Audio [Visible] 11 [],Audio [Visible] 22 [Audio [Invisible] 11 [],Audio [Invisible] 11 []],Audio [Visible] 7 [],Audio [Visible] 19 [],Audio [Visible] 15 [Audio [Invisible] 8 [],Audio [Visible] 4 [],Audio [Invisible] 14 []]]]]
      -- the greatest index should be less than or equal to the total number of indices
      let maxAudioIndex r' = maximum $ getIndicesWith (rewriteIndices r') isAudio
      let numAudios r' = length $ getIndicesWith r' isAudio
      let nogaps r' = maxAudioIndex r' <= numAudios r'

      nogaps r `shouldBe` True

    prop "rewriteIndices: checks that rewriting yields ascending audio indices without gaps (csv version)" $
      \c ->
        -- consider only (non-empty) rows with audios
        let mrs = filter (\r -> length (getAudios r) > 0) <$> getRows c :: Maybe [Row]

            maxAudioIndex r' = maximum $ getIndicesWith (rewriteIndices r') isAudio
            numAudios r' = length $ getIndicesWith r' isAudio
            -- the greatest index should be less than or equal to the total number of indices
            nogaps r = maxAudioIndex r <= numAudios r

        in case mrs of
             Nothing -> True `shouldBe` True
             Just rs -> (if (not . null) rs then and (fmap nogaps rs) else True) `shouldBe` True

    it "rewriteIndices: checks that rewriting yields ascending audio indices without gaps (row version)" $ do

      rs' <- sample' (sized genRow) :: IO [Row]
      -- we consider only the cases where there are audios in the tree
      let rs = filter (\r -> length (getAudios r) > 0) rs'
      let maxAudioIndex r' = maximum $ getIndicesWith (rewriteIndices r') isAudio
      let numAudios r' = length $ getIndicesWith r' isAudio
      -- the greatest index should be less than or equal to the total number of indices
      let nogaps r = maxAudioIndex r <= numAudios r
      -- E.g. consider the following lists of indices (which have been distributed ascendingly 
      -- but given the possiblity of duplicates may not occur in a strictly ascending order):
      -- 1,2,3,4: maxAudioIndex = 4 <= numAudios = 4 (correctly distributed); 
      -- 1,2,1,3: maxAudioIndex = 3 <= numAudios = 4 (correctly distributed);
      -- 1,2,1,4: maxAudioIndex = 5 <= numAudios = 4 (incorrectly distributed as 3 was jumped);
      and (fmap nogaps rs) `shouldBe` True

    it "rewriteIndices: checks that rewriting yields ascending insert indices without gaps" $ do

      rs' <- sample' (sized genRow) :: IO [Row]
      -- we consider only the cases where there are inserts in the tree
      let rs = filter (\r -> length (getInserts r) > 0) rs'
      let maxInsertIndex r' = maximum $ getIndicesWith (rewriteIndices r') isInsert
      let numInserts r' = length $ getIndicesWith r' isInsert
      -- the greatest index should be less than or equal to the total number of indices
      let nogaps r = maxInsertIndex r <= numInserts r

      and (fmap nogaps rs) `shouldBe` True

    it "rewriteIndices: checks that rewriting yields ascending cloze indices without gaps" $ do

      rs' <- sample' (sized genRow) :: IO [Row]
      -- we consider only the cases where there are clozes in the tree
      let rs = filter (\r -> length (getClozes r) > 0) rs'
      let maxClozeIndex r' = maximum $ getIndicesWith (rewriteIndices r') isCloze
      let numClozes r' = length $ getIndicesWith r' isCloze
      -- the greatest index should be less than or equal to the total number of indices
      let nogaps r = maxClozeIndex r <= numClozes r

      and (fmap nogaps rs) `shouldBe` True


    prop "checks composition of updating and rewriting indices" $ --do
      let prop_update_rewrite c = -- we don't count in the indices embedded in Audio and AudioInsert
            (rewriteIndices . updateDefIndsConst) (bareBones c) == rewriteIndices (bareBones c)
      in prop_update_rewrite
      --quickCheck $ withMaxSuccess 20 prop_update_rewrite

-- gets 'old' (non-zero) indices
myOlds :: (Markup -> Bool) -> CSV -> [Int]
myOlds pred c = [let Just i = getIndex m in i | m <- flattenAudios c, pred m]

-- checks that the number of duplicates stays the same after update
preserveDuplicates :: [Int] -> [Int] -> Bool
preserveDuplicates is js = countDuplicates is == countDuplicates js
  where
    countDuplicates :: [Int] -> Int
    countDuplicates xs =
      length $ filter (\xs' -> length xs' >= 2) $ group (sort $ filter (/= 0) xs)

-- gets the indices of markup elements that respect pred and have contents ms
getIndexWithContent :: CSV -> (Markup -> Bool) -> [Markup] -> [Int]
getIndexWithContent c pred ms =
  let targets = filter (\m -> pred m && m `hasContent` ms) (flattenAudios c)
      hasContent (Audio _ _ mconts) mconts' = mconts == mconts'
      hasContent (AudioInsert _ mconts) mconts' = mconts == mconts'
  in getIndicesWith (Cell targets) pred
