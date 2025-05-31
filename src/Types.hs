{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import RIO hiding (many,some,try)
import RIO.Process
import Text.Megaparsec


-- some plumbing provided by default

-- Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

-- my types

type Parser = Parsec Void String

data Markup = Cloze Int String
            | Bold String
            | Italic String
            | Regular String
            | Audio [Attr] Int [Markup]
            | AudioInsert Int [Markup]
            | MathInline String
            | MathBlock String
            | EndCell
            | EndRow
            deriving (Show,Eq,Generic)


{- NFData instances are needed to force complete evaluation.
NB: Generic instances on all the underlying types are also
needed as well as the language extension DeriveGeneric at the
very top of the file
-}

instance NFData Markup

data Attr = Visible
          | Invisible
          deriving (Show,Eq,Generic)

instance NFData Attr

data CSV = Cell [Markup]
         | Row [Cell]
         | Csv [Row]
         deriving (Show,Eq,Generic)

instance NFData CSV

type Cell = CSV
type Row = CSV

