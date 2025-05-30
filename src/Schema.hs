{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Schema where

import RIO hiding (many,some,try)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Test.QuickCheck
import Data.List (intercalate)

import Types
import Generators
import Util


-- generate specific csv index-structure from schematic string, e.g. A1 I2 C3

data Schema = A Int
            | I Int
            | C Int
            | SC [Schema] -- cell schema
            | SR [Schema] -- row schema
            | SCSV [Schema] -- csv schema
            deriving (Eq)

instance Show Schema where
  show (A i) = "A" ++ show i
  show (I i) = "I" ++ show i
  show (C i) = "C" ++ show i
  -- show (SC ss) =
  show c = show c

fill :: Parser String
fill = many (spaceChar <|> punctuationChar)

audioSchematon :: Parser Schema
audioSchematon = do
  _ <- char' 'a' 
  d <- decimal
  _ <- fill
  return (A d)
  <?> "audioSchematon"

insertSchematon :: Parser Schema
insertSchematon = do
  _ <- char' 'i'
  d <- decimal
  _ <- fill
  return (I d)
  <?> "insertSchematon"

clozeSchematon :: Parser Schema
clozeSchematon = do
  _ <- char' 'c' 
  d <- decimal
  _ <- fill
  return (C d)
  <?> "clozeSchematon"

schematon :: Parser Schema
schematon =
  choice [ audioSchematon
         , insertSchematon
         , clozeSchematon
         ] <?> "schematon"

schemata :: Parser [Schema]
schemata = many schematon <* eof



-- nr is #rows, nc is #cells per row, and str is the schema string
genFromSchemaCSV :: Int -> Int -> String -> IO CSV
genFromSchemaCSV nr nc str = do
  ms <- genFromSchema str
  -- collects chunks of length n from xs
  let groupsOf :: Int -> [a] -> [[a]]
      groupsOf _ [] = []
      -- if xs is not exactly divisible by n, the last cell will contain x markups, where n < x < 2n
      groupsOf n xs | length xs < 2 * n = xs : []
                    | otherwise         = take n xs : groupsOf n (drop n xs)
  let numMarkupsPerCell = length ms `div` nr `div` nc
  let cells = Cell <$> groupsOf numMarkupsPerCell ms
  let rows = Row <$> groupsOf nc cells
  return $ Csv rows
-- NB: the results will not be exact unless there the number of schematons (e.g. A1 or C3)
-- is perfectable divisible among the numbers of rows (nr) and cells (nc) given as arguments


-- generate markups from schema-string
genFromSchema :: String -> IO [Markup]
genFromSchema str =
  case parse schemata "" str of
    Right schms -> genFromSchema' schms
    _           -> return []

-- generate markups from elements of the Schema type
genFromSchema' :: [Schema] -> IO [Markup]
genFromSchema' schms = do
  -- markups to be embedded under Audio and AudioInsert
  ms <- generate unindexedMarkups3
  -- string to be embedded under Cloze
  str <- generate shortString
  -- markup to be interspersed between indexed elements
  m <- generate genSubmarkup' :: IO Markup
  let go [] = []
      go (x:xs) = 
        case x of
          A n -> Audio [] n ms : m : go xs
          I n -> AudioInsert n ms : m : go xs
          C n -> Cloze n str : m : go xs
          SC _ -> error "Cell schema not supported in this context"
          SR _ -> error "Row schema not supported in this context" 
          SCSV _ -> error "CSV schema not supported in this context"
  return $ go schms
    where
      unindexedMarkups3 :: Gen [Markup]
      unindexedMarkups3 = vectorOf 3 genSubmarkup'
      shortString :: Gen String
      shortString = vectorOf 6 arbitraryASCIIChar


getSchemaString :: CSV -> String
getSchemaString c = intercalate " " $
  filter (not . null) $ getSchematonString <$> unwrapCSV c


getSchematonString :: Markup -> String
getSchematonString (Audio _ i _) = "A" ++ show i
getSchematonString (AudioInsert i _) = "I" ++ show i
getSchematonString (Cloze i _) = "C" ++ show i
getSchematonString _ = ""

