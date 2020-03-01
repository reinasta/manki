{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import Prelude (read,sequence,repeat,(!!),maximum,head,last,tail,span,putStrLn,foldr1)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer (decimal)

--remove after testing
import Test.QuickCheck

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


rowEnd :: Parser Markup
rowEnd = doubleNewline $> EndRow

eofChar :: Parser Char
eofChar = eof $> ' '

eofStr :: Parser String
eofStr = eof $> ""

wordChar :: Parser Char
wordChar = no2newlines >>
  choice [ try nonmarker
         , try noDelimiterChar
         , try singleNewline
         , try noInvisibleAudio
         , try noInsertDigit
         --, try noAudioInsert'
         , noAudioInsert
         ] <?> "wordChar"
  where
    no2newlines = notFollowedBy doubleNewline
    nonmarker = noneOf "*_~@\n|0123456789-"
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
  choice [ try nonmarker
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
    nonmarker = noneOf "*_~@\n|>0123456789-"
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
nonempty xs = filter isNonEmptyCsv xs

-- string version of bold
bold' :: Parser String
bold' = boldMarker *> wordChar `manyTill` (lookAhead boldMarker) <* boldMarker

-- typed bold
bold :: Parser Markup
bold = fmap Bold bold' <?> "bold"

-- string version of bold
italic' :: Parser String
italic' = italicMarker *> wordChar `manyTill` (lookAhead italicMarker) <* italicMarker

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
  string "|>"
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
             , eofStr
             ]
    markerStr = (oneOf "~_*@") $> ""
    audioStr = audio $> ""
    audioInsertStr = audioInsertMarker $> ""

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
             , eofStr
             ]
    markerStr = (oneOf "~_*@") $> ""
    audioStr = audio $> ""
    audioInsertStr = audioInsertMarker $> ""



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
  choice [ try audio
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

{-
Using rowWith (this module), copyMarkupWith (Audio module),
updateIndicesWith and rewriteIndicesWith (Indices module), one can
construct filters that shape the working of the csv parser.

There are a couple of reasonable ways of putting these filters
together.

- the copyMarkupWith function affects which Audio markups
(i.e. the markup contents under the Audio label) get copied
at the insertion place in the csv string -- the insertion
place is marked by the AudioInsert element.
    - if, for instance, we copy all the Audio elements,
    including those zero- and nonzero-indexed, then any
    Audio markup that does not receive an index (in the
    user's input text file) ends up going into the first
    zero-indexed audio insert place (i.e. in an AudioInsert
    element). This could be useful in case we use very few
    markers, e.g.

        @~Look~@ an obscure word @~up~@
        The hidden phrasal verb is: |>

    The string "Look up" will go into the place of |>. Had
    we not copied all audio indexed elements, but only those
    with (non-zero) indices provided by the user, the two
    words making up the string "Look up" would not have been
    copied at the same insert place.
    - so far we worked under the assumption that indices
    have not been re-assigned to markup elements. Which is
    to say that in the AST, there are either default,
    zero-indices on elements (clozes, audios, audio-inserts)
    provided by default by the parser or non-zero indices
    provided explicitly by the user. But we can update these
    indices. We can use updateIndices to update default
    indices and leave the non-default ones with the original
    values provided by the user. Or we can use rewriteIndices
    to rewrite all the values, making sure that elements
    that had the same index (co-indexed elements) still
    share an index, although not necessarily the previous
    one. This overwrites the user's indices, but provided that
    audio markups are copied (via copyMarkupWith) before
    the index update, no cross-references (co-indexation)
    is lost.
    - now, there is not stringent reason to preserve the user's
    indices on audio elements, since these indices are not
    part of the output csv and they would have had their
    effects (namely the construction of audio strings and
    their placement at the particular place in the csv's
    cells) by the time the csv file is written. In contrast,
    it is more significant to have cloze indices set by the
    user, as these appear in the csv output and eventually
    in Anki (once the csv is successfully imported). To
    turn on the rewriting of indices for audio and insert
    elements, we filter the csv parse with the function
    rewriteInsertIndices and rewriteAudioIndices, which
    are just versions of rewriteIndicesWith (with different
    predicates as the second argument; the first argument
    being the csv element).
    - that said, I adopted as a default a (partial) update
    of indices, including those of clozes. I just filter the
    csv element with updateIndices. But we can mix and match
    using the library.


-}

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


smp = sample' (resize 100 arbitrary) :: IO [String]