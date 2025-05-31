{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ResultsSpec where


import Prelude
import Data.Aeson (Value, decode, toJSON) -- Modified: Added Value, decode, and toJSON for clarity, though toJSON is often in Prelude via Aeson
import GHC.Generics
import Data.Text as T (unpack)
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.ByteString.Lazy.Char8 as LBS (pack) -- Added for converting String to Lazy ByteString


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

      -- Get the Aeson Value from your data structure
      let actualValue = toJSON res1

      let desired_json_string = "{\"csvData\":{\"csvName\":\"example.csv\",\"csvRows\":[{\"row\":\"\\\"r1 cell one\\\", \\\"r1 cell two\\\", \\\"r1 cell three\\\"\",\"audios\":[\"pronounce this\",\"this should become a sound\"]},{\"row\":\"\\\"r2 cell one\\\", \\\"r2 cell two\\\", \\\"r2 cell three\\\"\",\"audios\":[\"how does this sound?\",\"sound audio\"]}],\"csvString\":\"\\\"r1 cell one\\\", \\\"r1 cell two\\\", \\\"r1 cell three\\\"\\n\\\"r2 cell one\\\", \\\"r2 cell two\\\", \\\"r2 cell three\\\"\"}}"

      -- Decode the desired JSON string into an Aeson Value
      -- decode expects a Lazy ByteString, so we pack the String
      let mExpectedValue :: Maybe Value
          mExpectedValue = decode (LBS.pack desired_json_string)

      case mExpectedValue of
        Nothing -> expectationFailure $ "Failed to parse desired_json_string into Aeson Value: " ++ desired_json_string
        Just expectedValue -> actualValue `shouldBe` expectedValue

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
