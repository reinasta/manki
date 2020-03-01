{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Interface
--import Run
--import RIO.Process
--import qualified Paths_manki

main :: IO ()
main = runManki
