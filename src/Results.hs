{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Results where

import Prelude
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import GHC.Generics

type RowString = String
type AudioString = String

data Results =
  Results { csvName :: String -- the name of the csv file
          , csvRows :: [(RowString,[AudioString])] -- row strings with their respective audio strings
          , csvString :: String -- the entire csv string
          } deriving (Generic,Show)

-- the JSON result is built around the ouptut csv file and contains data about csv (filename, rows, audios etc)
instance ToJSON Results where
  toJSON (Results name rs content) =
    object ["csvData" .= object [ "csvName" .= name
                                , "csvRows" .= getRowObjects rs
                                , "csvString" .= content
                                ]
           ]
      where
        getRowObjects [] = []
        getRowObjects ((_rstr,as):rs') =
          Object (KM.fromList [("row", toJSON _rstr), ("audios", toJSON as)]) : getRowObjects rs'

-- writing a JSON string (rather than ByteString)
getJSON :: Results -> String
getJSON = show . encode

getJsonArray :: [String] -> String
getJsonArray = show . toEncodingList
