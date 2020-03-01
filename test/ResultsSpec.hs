{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ResultsSpec where


import Prelude
import Data.Aeson
import GHC.Generics
import Data.Text as T (unpack)
import Test.Hspec
import Test.Hspec.QuickCheck


import Types
import Results
import Util (getAudios)
import Write (stringifyElem)


spec :: Spec
spec = do
  describe "Results checks" $ do

    it "writes results as JSON" $ do

      let res1 = Results { csvName = "example.csv"
                         , csvRows = [ ( "\"r1 cell one\", \"r1 cell two\", \"r1 cell three\""
                                       , ["pronounce this", "this should become a sound"]
                                       )
                                     , ( "\"r2 cell one\", \"r2 cell two\", \"r2 cell three\""
                                       , ["how does this sound?", "sound audio"]
                                       )
                                     ]
                         , csvString = "\"r1 cell one\", \"r1 cell two\", \"r1 cell three\"\n\"r2 cell one\", \"r2 cell two\", \"r2 cell three\""
                         }

      let res1_json = encode res1

      let desired_res = "{\"csvData\":{\"csvName\":\"example.csv\",\"csvRows\":[{\"row\":\"\\\"r1 cell one\\\", \\\"r1 cell two\\\", \\\"r1 cell three\\\"\",\"audios\":[\"pronounce this\",\"this should become a sound\"]},{\"row\":\"\\\"r2 cell one\\\", \\\"r2 cell two\\\", \\\"r2 cell three\\\"\",\"audios\":[\"how does this sound?\",\"sound audio\"]}],\"csvString\":\"\\\"r1 cell one\\\", \\\"r1 cell two\\\", \\\"r1 cell three\\\"\\n\\\"r2 cell one\\\", \\\"r2 cell two\\\", \\\"r2 cell three\\\"\"}}"

      res1_json `shouldBe` desired_res

    it "extracts audios and writes resulting list as JSON array" $ do

      let mycsv = Csv [Row [Cell [ Audio [] 1 [Bold "one"]
                                 , Audio [] 2 [Italic "two"]
                                 , Audio [] 3 [Regular "three"]
                                 , Audio [] 4 [Cloze 7 "four"]
                                 , Audio [] 5 [Bold "five"]
                                 ]
                           ]
                      ]

      let myjson = "\"[\\\"one\\\",\\\"two\\\",\\\"three\\\",\\\"four\\\",\\\"five\\\"]\""
      --let myjson = "[\"one\",\"two\",\"three\",\"four\",\"five\"]"

      getJsonArray (stringifyElem <$> getAudios mycsv) `shouldBe` myjson

{- JSON sample: desired_res (see above)

"{\"csvData\":{
    \"csvName\":\"example.csv\",
    \"csvRows\":[{\"row\":\"\\\"r1 cell one\\\", \\\"r1 cell two\\\", \\\"r1 cell three\\\"\",
                  \"audios\":[\"pronounce this\",\"this should become a sound\"]
                 }
                ,{\"row\":\"\\\"r2 cell one\\\", \\\"r2 cell two\\\", \\\"r2 cell three\\\"\"
                ,\"audios\":[\"how does this sound?\",\"sound audio\"]}
                ]
    ,\"csvString\":\"\\\"r1 cell one\\\", \\\"r1 cell two\\\", \\\"r1 cell three\\\"\\n\\\"r2 cell one\\\", \\\"r2 cell two\\\", \\\"r2 cell three\\\"\"}}"

-}
