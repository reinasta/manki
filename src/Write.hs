{-# LANGUAGE NoImplicitPrelude #-}

module Write where

import RIO hiding (try,many,some)
import Data.Char (isSpace, isAlphaNum)
import Data.List (intersperse,intercalate,head,tail,words)
import Test.Hspec.Core.Util (strip)

import Types
import Util (deleteEmptyCSV)


-- get something closer to the original text strings; NB: indices will likely differ

stringifyBack :: CSV -> String
stringifyBack = stringifyCsvBack . deleteEmptyCSV
-- NB: we delete empty rows and empty cells

stringifyCsvBack :: CSV -> String
-- we strip whitespace at the beginning of Cells as it is not allowed
stringifyCsvBack (Cell ms) = strip (concatMap stringifyMarkupBack ms) ++ "\n---\n"
stringifyCsvBack (Row cs) | null cs        = ""
                          | length cs == 1 = let Cell ms = head cs
                                             in strip(concatMap stringifyMarkupBack ms) ++ "\n\n"
                          | otherwise      = stringifyCsvBack (head cs) ++
                                               stringifyCsvBack (Row $ tail cs)
stringifyCsvBack (Csv rs) = stripAllButDelimiters $ concatMap stringifyCsvBack rs
  where
    stripAllButDelimiters [] = []
    stripAllButDelimiters chars@(c:cs)
      -- if chrs starts with a delimiter, we leave it as it is and check the end
      | take 5 chars == "\n---\n" = reverse $ stripAllButDelimiters $ reverse chars
      | isSpace c = stripAllButDelimiters cs
      | (not . isSpace) c = chars

stringifyMarkupBack :: Markup -> String
stringifyMarkupBack (Regular str) = str
stringifyMarkupBack (Bold str) = "*" ++ str ++ "*"
stringifyMarkupBack (Italic str) = "_" ++ str ++ "_"
stringifyMarkupBack (Cloze i str) = "@" ++ str ++ "@" ++ showCustomIndex i
stringifyMarkupBack (AudioInsert i _) = showCustomIndex i ++ "|> "
stringifyMarkupBack (Audio as i ms)
  | Invisible `elem` as = "--" ++ "@" ++ concatMap stringifyMarkupBack ms ++ "@" ++ showCustomIndex i
  | otherwise           = "@" ++ concatMap stringifyMarkupBack ms ++ "@" ++ showCustomIndex i
stringifyMarkupBack _ = ""

showCustomIndex :: Int -> String
showCustomIndex i = if i == 0 then "" else show i

-- get the strings embedded in Markup elements (no markup is preserved)
stringifyElem :: Markup -> String
stringifyElem (Regular str) = str
stringifyElem (Italic str) = str
stringifyElem (Bold str) = str
stringifyElem (Cloze _ str) = str
stringifyElem (Audio as _ ms) | Invisible `elem` as = ""
                              | otherwise           = concatMap stringifyElem ms
stringifyElem (AudioInsert _ ms) = concatMap stringifyElem $ intersperse (Regular " ") ms
stringifyElem _ = ""

-- anki strings

ankiStringifyElem :: Markup -> String
ankiStringifyElem m@(Regular _) = csvFilters (stringifyElem m)
ankiStringifyElem m@(Italic _) = "<i>" ++ csvFilters (stringifyElem m) ++ "</i>"
ankiStringifyElem m@(Bold _) = "<b>" ++ csvFilters (stringifyElem m) ++ "</b>"
ankiStringifyElem m@(Cloze i _) =
  "{{c" ++ show i ++ "::" ++ csvFilters (stringifyElem m) ++ "}}"
ankiStringifyElem (Audio _ _ ms) = csvFilters (concatMap ankiStringifyElem ms)
ankiStringifyElem m@(AudioInsert _ _) =
  let content = audioFilename (stringifyElem m)
  in "[sound:" ++ content ++ ".mp3]" -- anki markup for sound files
ankiStringifyElem _ = ""

-- 5_word_content_like_this (no punctuation, symbols, or newlines)
audioFilename :: String -> String
audioFilename str = intercalate "_" $ words $ filter (\c -> isAlphaNum c || isSpace c) str

-- some filters

csvFilters :: String -> String
csvFilters str = escapeDoubleQuote . newlineToBrTag $ str

escapeDoubleQuote :: String -> String
escapeDoubleQuote [] = []
escapeDoubleQuote (c:cs) | c == '"'  = "\"\"" ++ escapeDoubleQuote cs
                         | otherwise = c : escapeDoubleQuote cs

newlineToBrTag :: String -> String
newlineToBrTag [] = []
newlineToBrTag (c:cs) | c == '\n' = "<br>" ++ newlineToBrTag cs
                      | otherwise = c : newlineToBrTag cs

-- gets invisible audio elements (those marked up with -@hidden@)
ankiStringifyInvisible :: Markup -> Maybe String
ankiStringifyInvisible (Audio as _ ms) | Invisible `elem` as = Just $ concatMap stringifyElem ms
ankiStringifyInvisible _ = Nothing

-- converts the AST to a csv string
ankiStringifyCSV :: CSV -> String
ankiStringifyCSV (Cell ms) = "\"" ++ concatMap ankiStringifyElem ms ++ "\""
ankiStringifyCSV (Row cs) =  intercalate "," (fmap ankiStringifyCSV cs)
ankiStringifyCSV (Csv rs) = intercalate "\n" (fmap ankiStringifyCSV rs)

