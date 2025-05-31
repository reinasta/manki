{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import Prelude (read)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer (decimal)

import Types
import Audio (copyAudios,copyUnmergedAudios,removeEmptyAudios)
import Indices (updateIndices,rewriteIndices)
import RIO hiding (try,many,some)


-- a marker is a symbol that is adjacent to an alpha-numeric character
makeSymmetricMarker :: String -> Parser String
makeSymmetricMarker str = try leftMarker <|> rightMarker
  where
    leftMarker = string str <* lookAhead alphaNumChar
    rightMarker = notFollowedBy spaceChar *> string str

italicMarker :: Parser String
italicMarker = makeSymmetricMarker "_"

boldMarker :: Parser String
boldMarker = makeSymmetricMarker "*"

-- Helper for escaped characters
escapedCharP :: Char -> Parser Char
escapedCharP c = char '\\' *> char c

-- Math parsers
mathInline :: Parser Markup
mathInline = try (do
  _ <- char '$'
  -- Ensure it's a single '$', not '$$'
  notFollowedBy (char '$') <?> "unexpected '$$' for inline math; use '$...$' for inline or '$$...$$' for block"
  -- Content parser:
  -- 1. Try to parse "\\$" as a literal string.
  -- 2. Otherwise, parse any character that is not '$' or a newline.
  let contentChar = try (string "\\$") <|> fmap (:[]) (noneOf "$\n\r")
  contentParts <- manyTill contentChar (char '$' <?> "unclosed inline math: missing closing '$' (or did you mean to escape a dollar sign with '\\$'?)")
  return (MathInline (concat contentParts))) <?> "mathInline"

mathBlock :: Parser Markup
mathBlock = try (do
  _ <- string "$$"
  -- Content parser for block math (allows newlines):
  -- 1. Try to parse "\\$" as a literal string.
  -- 2. Otherwise, parse any character that is not '$'.
  let contentChar = try (string "\\$") <|> fmap (:[]) (noneOf "$") -- Allows newlines
  contentParts <- manyTill contentChar (string "$$" <?> "unclosed block math: missing closing '$$'")
  return (MathBlock (concat contentParts))) <?> "mathBlock"


rowEnd :: Parser Markup
rowEnd = doubleNewline $> EndRow

eofChar :: Parser Char
eofChar = eof $> ' '

eofStr :: Parser String
eofStr = eof $> ""

wordChar :: Parser Char
wordChar = no2newlines >>
  choice [ try (escapedCharP '$') -- Allow escaped dollar sign
         , try nonmarker
         , try noDelimiterChar
         , try singleNewline
         , try noInvisibleAudio
         , try noInsertDigit
         --, try noAudioInsert'
         , noAudioInsert
         ] <?> "wordChar"
  where
    no2newlines = notFollowedBy doubleNewline
    nonmarker = noneOf "*_~@\n|0123456789-$" -- $ is not a nonmarker; it's special or escaped
    singleNewline = newline <* notFollowedBy (newlineStr <|> delTail)
    noInvisibleAudio = char '-' <* notFollowedBy (string "-@" <|> string "@")
    noDelimiterChar = newline <* notFollowedBy delTail
    -- some helpers
    delTail = twoOrMore '-' <* newline
    newlineStr = (try newline <|> char '\r') $> ""

    noAudioInsert = char '|' <* notFollowedBy (char '>') -- NEW
    --noAudioInsert' = notFollowedBy (char '|') *> (char '>' :: Parser Char) -- NEW
    noInsertDigit = digitChar <* notFollowedBy (many digitChar *> string "|>")

-- a more permissive version of wordChar; used to parse inside clozes, audios, bold etc.
wordCharPermissive :: Parser Char
wordCharPermissive = no2newlines >>
  choice [ try (escapedCharP '$') -- Allow escaped dollar sign
         , try nonmarker
         , try singleNewline
         --, try noInvisibleAudio
         , try noDelimiterChar
         , try (char '-') -- in Audios, dashes are allowed
         , try noInsertDigit
         , try noAudioInsert'
         , noAudioInsert
         ] <?> "wordChar"
  where
    no2newlines = notFollowedBy doubleNewline
    nonmarker = noneOf "*_~@\n|>0123456789-" -- $ is not a nonmarker here either
    singleNewline = newline <* notFollowedBy (try newline <|> char '\r')
    --noInvisibleAudio = char '-' <* notFollowedBy (string "-@" <|> string "@")
    noDelimiterChar = newline <* notFollowedBy (twoOrMore '-')

    noAudioInsert = char '|' <* notFollowedBy (char '>') -- NEW
    noAudioInsert' = notFollowedBy (char '|') *> (char '>' :: Parser Char) -- NEW
    noInsertDigit = digitChar <* notFollowedBy (many digitChar *> string "|>")


-- determine whether Csv element is empty
isEmptyCsv :: CSV -> Bool
isEmptyCsv (Cell xs) = null xs
isEmptyCsv (Row xs) = null xs
isEmptyCsv (Csv xs) = null xs

isNonEmptyCsv :: CSV -> Bool
isNonEmptyCsv = not . isEmptyCsv

-- filter out emtpy csv elements
nonempty :: [CSV] -> [CSV]
nonempty = filter isNonEmptyCsv

-- string version of bold
bold' :: Parser String
bold' = boldMarker *> wordChar `manyTill` lookAhead boldMarker <* boldMarker

-- typed bold
bold :: Parser Markup
bold = fmap Bold bold' <?> "bold"

-- string version of bold
italic' :: Parser String
italic' = italicMarker *> wordChar `manyTill` lookAhead italicMarker <* italicMarker

-- typed italic
italic :: Parser Markup
italic = fmap Italic italic' <?> "italic"


cloze :: Parser Markup
cloze = do
  _ <- char '~'
  str <- wordChar `manyTill` lookAhead (char '~')
  _ <- char '~'
  n <- option 0 (read <$> some digitChar :: Parser Int)
  return (Cloze n str)
  <?> "cloze"

audioInsertMarker :: Parser Markup
audioInsertMarker = do
  maybeN <- optional decimal
  let n = fromMaybe 0 maybeN
  _ <- string "|>"
  --space
  return $ AudioInsert n []
  <?> "audioInsertMarker"

-- a delimiter consists of two or more dashes on their own line
fieldDelimiter :: Parser String
fieldDelimiter =
  let dash = char '-'
      twoDashes = string "--"
      nl = try crlf <|> newline $> ""
      dashes = nl *> twoDashes <* skipMany dash <* nl
  in (try dashes <|> eofStr) <?> "fieldDelimiter"

cellEnd :: Parser Markup
cellEnd = fieldDelimiter $> EndCell

-- string without markup
regular :: Parser Markup
regular = Regular <$> wordChar `someTill` end <?> "regular"
  where
    end = lookAhead $
      choice [ try markerStr
             , try audioStr
             , try fieldDelimiter
             , try doubleNewline
             , try audioInsertStr
             , try mathMarkerStr -- Definition updated below
             , eofStr
             ]
    markerStr = (oneOf "~_*@") $> ""
    audioStr = audio $> ""
    audioInsertStr = audioInsertMarker $> ""
    mathMarkerStr = (try (string "$$") <|> try (string "$" <* notFollowedBy (char '$'))) $> ""

-- a more permissive version of string without markup to be invoked inside audios, clozes etc.
regularPermissive :: Parser Markup
regularPermissive = Regular <$> wordCharPermissive `someTill` end <?> "regular"
  where
    end = lookAhead $
      choice [ try markerStr
             , try audioStr
             , try fieldDelimiter
             , try doubleNewline
             , try audioInsertStr
             , try mathMarkerStr -- Definition updated below
             , eofStr
             ]
    markerStr = (oneOf "~_*@") $> ""
    audioStr = audio $> ""
    audioInsertStr = audioInsertMarker $> ""
    mathMarkerStr = (try (string "$$") <|> try (string "$" <* notFollowedBy (char '$'))) $> ""



-- audio parser (contains markup elements)
audio :: Parser Markup
audio = do
  sign <- optional (char '-' <* many (char '-'))
  _ <- char '@'
  ms <- many markupPermissive -- markup `manyTill` lookAhead (char '@')
  _ <- char '@'
  maybeN <- optional decimal
  let n = fromMaybe 0 maybeN
  if sign == Just '-'
    then return (Audio [Invisible] n ms)
    else return $ Audio [Visible] n ms
  <?> "audio"


-- any element of type Markup
markup :: Parser Markup
markup =
  choice [ try mathBlock         -- Try block math first (prefix "$$")
         , try mathInline        -- Then inline math (prefix "$")
         , try audio
         , try regular
         , try italic
         , try bold
         , try cloze
         , audioInsertMarker
         ]

-- any element of type Markup; permissive of e.g. --@ after a staring audio markers (@)
markupPermissive :: Parser Markup
markupPermissive =
  choice [ try audio
         , try regularPermissive
         , try italic
         , try bold
         , try cloze
         , audioInsertMarker
         ]

cell :: Parser Cell
cell = Cell <$> markup `someTill` lookAhead end
  where
    end = try fieldDelimiter <|> try doubleNewline <|> eofStr

row :: Parser Row
row = rowUpdateAndCopy

-- used by default
rowUpdateAndCopy :: Parser Row
rowUpdateAndCopy = rowWith $ removeEmptyAudios . copyAudios . updateIndices

-- a nice, orderly index-rewriting
rowCopyAndRewrite :: Parser Row
rowCopyAndRewrite = rowWith $ removeEmptyAudios . rewriteIndices . copyAudios

-- for cases of multiple default audios and inserts
rowMultipleDefaults :: Parser Row
rowMultipleDefaults = rowWith $ removeEmptyAudios . rewriteIndices . copyUnmergedAudios

-- for mixed cases with defaults and explicit indices
rowMultipleMixed :: Parser Row
rowMultipleMixed = rowWith $ removeEmptyAudios . copyUnmergedAudios . rewriteIndices

-- parse a csv string and include only non-empty rows in the result
csv :: Parser CSV
csv = csvWith $ rowWith $ removeEmptyAudios . copyAudios . updateIndices

-- we update indices and copy audios into their insert places at row-level
rowWith :: (CSV -> CSV) ->  Parser Row
rowWith fltr = fltr . Row <$> (notFollowedBy eofStr >> cell `sepBy` fieldDelimiter)

-- parse a csv string and include only non-empty rows in the result
csvWith :: Parser Row -> Parser CSV
csvWith rw =
  let noTrailing = try (rw `sepBy` doubleNewline)
      ayTrailing = (rw `endBy` doubleNewline)
  in Csv . nonempty <$> (noTrailing <|> ayTrailing)
-- NB: strings ending in two or more newlines require `endBy` (unless previously stripped)

-- some helpers

doubleNewline :: Parser String
doubleNewline =
  (crlf >> crlf >> many crlf >> return "\r\n\r\n")
  <|> (newline >> newline >> many newline >> return "\n\n")

space :: Parser ()
space = skipMany whitespace

whitespace :: Parser Char
whitespace = oneOf " \t\f\v"

twoOrMore :: Char -> Parser String
twoOrMore c =
  let pc = char c
      pcs = many $ try pc
  in (pc *> pc *> pcs) *> pure [c,c]

twoOrMore' :: String -> Parser String
twoOrMore' str = do
  let pstr = string str
  skipCount 2 pstr
  _ <- many pstr
  return (str ++ str)

--used for testing

-- row and csv parsers that do not update indices (whose actual value is unpredictable
-- and not really significant); used only for testing
rowIgnoringIndices :: Parser Row
rowIgnoringIndices = copyAudios . Row <$> (notFollowedBy eofStr >> cell `sepBy` fieldDelimiter)

-- parse a csv string and include only non-empty rows in the result
csvIgnoringIndices :: Parser CSV
csvIgnoringIndices =
  let noTrailing = try (rowIgnoringIndices `sepBy` doubleNewline)
      ayTrailing = (rowIgnoringIndices `endBy` doubleNewline)
  in Csv . nonempty <$> (noTrailing <|> ayTrailing)


--smp :: IO [String]
--smp = sample' (resize 100 arbitrary) :: IO [String]