{-# LANGUAGE NoImplicitPrelude #-}
module Interface (runManki,selectUpdate) where

import Prelude
import Options.Applicative hiding (command,str)
import Data.Semigroup ((<>))
import Data.List (sort)
import System.FilePath
import Test.Hspec.Core.Util (strip)
import Text.Megaparsec

import Types hiding (Parser)
import Parser (csvWith,rowWith)
import Write
import Util (isInsert,isAudio,getIndicesWith,getInserts,isCloze)
import Audio (copyAudios,copyUnmergedAudios)
import Indices (updateIndices,rewriteIndices,rewriteIndicesWith)
import Results

-- commandline options
data CommandOpt =
  FileInput FilePath
  | FileOutput FilePath
  | AskCSV
  | AskJSON
  | AskAudios
  | AskManki
  deriving (Show, Eq)

-- general command type
data Command = Command
  { input :: CommandOpt
  , output :: CommandOpt
  , asksome :: CommandOpt
  } deriving (Show,Eq)


{- TO DO

- test on Windows and macOS
- better error type and error messages, perhaps with line number information
- csv validity check and correction, e.g. add NULL cells in case csv rows are unequal
- generate multiple rows out of a row with multiple audios (e.g. one for each audio)

-}

-- option and flag parsers

inputOpt :: Parser CommandOpt
inputOpt = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> value "" -- default value
  <> help "File to be converted to csv"
  )

outputOpt :: Parser CommandOpt
outputOpt = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILE"
  <> value "" -- default value
  <> help "Where to output the csv string (default: stdout)"
  )

csvFlag :: Parser CommandOpt
csvFlag = flag AskCSV AskCSV
  (  long "csv"
  <> help "Ask for a CSV string" )

jsonFlag :: Parser CommandOpt
jsonFlag = flag AskCSV AskJSON
  (  long "json"
  <> help "Ask for a JSON string representing a csv" )

audiosFlag :: Parser CommandOpt
audiosFlag = flag AskCSV AskAudios
  (  long "audios"
  <> help "Ask for a list of audio strings as JSON array" )

mankiFlag :: Parser CommandOpt
mankiFlag = flag AskCSV AskManki
  (  long "manki"
  <> help "Ask for Manki's native AST" )



askFlag :: Parser CommandOpt
askFlag = csvFlag <|> jsonFlag <|> audiosFlag <|> mankiFlag



command :: Parser Command
command = Command <$> inputOpt <*> outputOpt <*> askFlag


runManki :: IO ()
runManki = do
  cmmd <- execParser withInfo
  let asked = asksome cmmd
  let FileInput inputFileArg = input cmmd
  let FileOutput outputFileArg = output cmmd
  -- deal with input
  let ioStrContent filename | null filename = getContents
                            | otherwise     = readFile filename
  str <- ioStrContent inputFileArg
  -- get results
  let Right csvtree = parse (csvWith $ rowWith id) "" (strip str)
  -- updated csv tree (we move audio markups into insert places and perhaps update indices)
  let updatedCsvTree = selectUpdate csvtree
  let csvStr = ankiStringifyCSV updatedCsvTree
  let rowASTs = let Csv rs = updatedCsvTree in rs
  let rowAndAudioStrings = -- we reformat audio insert contents to get the spaces btw words right
       [(ankiStringifyCSV r , unwords . words . stringifyElem <$> getInserts r) | r <- rowASTs]
  -- only *inserted* audios are listed; so we must update the csv tree (i.e. move audios) first
  let audioStrings = concatMap snd rowAndAudioStrings
  let jsonString = getJSON $ Results (inputFileArg ++ "_in_json") rowAndAudioStrings csvStr
  -- deal with output; first decide the output filename and extension (if none were given)
  let getOutputFilename outfile ext | null outfile = replaceExtension inputFileArg ext
                                    | otherwise     = outfile
  -- select which result to output
  let selectResultString demand = 
        case demand of
          AskManki  -> show $ updatedCsvTree
          AskJSON   -> jsonString
          AskAudios -> getJsonArray audioStrings
          _         -> csvStr
  -- select which extension to use for the output file (if any is required)
  let selectExtension demand = 
        case demand of
          AskManki  -> ".manki"
          AskJSON   -> ".json"
          AskAudios -> ".json"
          _         -> ".csv"

  -- output file only if an output filename was given, and use stdout otherwise
  if null outputFileArg
     then putStrLn $ selectResultString asked
     else writeFile (getOutputFilename outputFileArg $ selectExtension asked) (selectResultString asked)

-- entry point for the parser of command line arguments
withInfo :: ParserInfo Command
withInfo = info (command <**> helper)
  ( fullDesc
  <> progDesc "Convert text FILE to a csv FILE; or get a corresponding JSON FILE; or get the AUDIO strings"
  <> header "Manki converts lightly marked up text files to csv files to be imported in Anki" )


-- selecting the right index-update & markup-copying functions; see use-cases below

selectUpdate :: CSV -> CSV
selectUpdate r@(Row _) =
  let audioIndices = sort $ getIndicesWith r isAudio
      insertIndices = sort $ getIndicesWith r isInsert
      -- determines whether audios and inserts are one-to-one coindexed
      eqIndices inds1 inds2 = inds1 == inds2
      -- determines whether the numbers of default audios and inserts are > 1
      manyDefaults inds1 inds2 =
        let manyZeros xs = length $ filter (== 0) xs
        in manyZeros inds1 > 1 && manyZeros inds2 > 1
  in case () of _ | manyDefaults audioIndices insertIndices -> rewriteIndices $ copyUnmergedAudios r
                  | eqIndices audioIndices insertIndices    -> (flip rewriteIndicesWith) isCloze $ copyAudios r
                  | otherwise                               -> updateIndices $ copyAudios r
selectUpdate (Csv rs) = Csv $ fmap selectUpdate rs
selectUpdate c@(Cell _) = c
-- NB: a cell update (with data drawn from that cell) would be impoverished,
-- so we update the cells at row level (drawing data from the entire row)



{- USE-CASES

if there are more than one zero-indexed Audios and AudioInserts, parse row with copyUnmergedAudios

A0 A3 A0
I0 I0 I3

if the lists of indices of Audios and AudioInserts are equal (without removing duplicates), proceed 
without changing any index:

A0 A1 A2
I0 I1 I2

NB: this case is partially overlapping with the previous one

if there is only one default AudioInsert and two or more (coindexed) default Audios, then parse row 
with copyAudios (which merges coindexed audios and plants them at the coindexed insert place):

A0 A0 A7
I0 I7

NB: this case is singled out for its significance; otherwise it should be collapsed into the next one,
and will be in the actual code.

for all the other cases, use copyAudios

A* A* A*
I* I* I*

-}
