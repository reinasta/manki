{-# LANGUAGE NoImplicitPrelude #-}

module Validity where

import Prelude
import Data.List (nub,sort)
import Data.Map.Strict (Map,fromList, keys, filterWithKey)


import Types
import Util

-- counting cell- and row- and csv contents

-- ordering rows and cells

type Position = Int
type Count = Int
data Info = Info Position Count deriving (Show,Eq,Ord)


-- list the csv rows with more or less than the required number of cells n
withRowsWithout :: CSV -> Int -> [Info]
withRowsWithout csv n =
  let countDiffTo (Info _ c) i = c /= i
      countCsv :: CSV -> Map Info (Map Info Cell)
      countCsv (Csv rs) =
        fromList [(Info p (countCells r), cellInfo r) | (r,p) <- zip rs [1..]]
      countCsv _ = fromList [] -- Handle Cell and Row cases
      
      countCells :: Row -> Int
      countCells (Row cs) = length cs
      countCells _ = 0 -- This case should never be reached
      
      cellInfo :: Row -> Map Info Cell
      cellInfo (Row cs) =
        fromList [(Info p (countCellContents c), c) | (c,p) <- zip cs [1..]]
      cellInfo _ = fromList [] -- This case should never be reached
      
      countCellContents :: Cell -> Int
      countCellContents (Cell cs) = length cs
      countCellContents _ = 0 -- Handle Row and Csv cases

  in keys $ filterWithKey (\k _ -> k `countDiffTo` n) (countCsv csv)


equalRows :: CSV -> Bool
equalRows (Csv []) = True
equalRows (Csv rs) = allEqual $ fmap rowLength rs
  where
    allEqual :: [Maybe Int] -> Bool
    allEqual ints = if length (nub ints) == 1 then True else False
    rowLength :: CSV -> Maybe Int
    rowLength (Row cs) = Just $ length cs
    rowLength _ = Nothing
equalRows _ = True


-- checks if the audios and inserts in a csv have matching indices; allows many-to-one coindexation relations
audioInsertMatching :: CSV -> Bool
audioInsertMatching c@(Cell _) = nub (sort $ getIndicesWith c isInsert) == nub (sort $ getIndicesWith c isAudio)
audioInsertMatching r@(Row _) = nub (sort $ getIndicesWith r isInsert) == nub (sort $ getIndicesWith r isAudio)
audioInsertMatching (Csv rs) = and $ audioInsertMatching <$> rs
--NB: we check the property at row-level

-- checks audio-insert index matching; more strict then the above as it requires a one-to-one coindexation -- relation; duplicate pairs allowed
audioInsertMatching' :: CSV -> Bool
audioInsertMatching' c@(Cell _) = sort (getIndicesWith c isInsert) == sort (getIndicesWith c isAudio)
audioInsertMatching' r@(Row _) = sort (getIndicesWith r isInsert) == sort (getIndicesWith r isAudio)
audioInsertMatching' (Csv rs) = and $ audioInsertMatching' <$> rs
--NB: we check the property at row-level

