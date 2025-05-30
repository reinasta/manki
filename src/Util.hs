{-# LANGUAGE NoImplicitPrelude #-}
module Util where


import Prelude ((!!),maximum,foldr1)
import Types
import RIO hiding (try,many,some)



-- unwrap audio elements
unwrapAudioElems :: Markup -> Maybe [Markup]
unwrapAudioElems (Audio _ _ ms) = Just ms
unwrapAudioElems _ = Nothing

-- unwrap CSV elements
unwrapCSV :: CSV -> [Markup]
unwrapCSV (Cell ms) = ms
unwrapCSV (Row cs) = concatMap unwrapCSV cs
unwrapCSV (Csv rs) = concatMap unwrapCSV rs

unwrapCell :: Cell -> [Markup]
unwrapCell (Cell ms) = ms
unwrapCell _ = []

unwrapRow :: Row -> [[Markup]]
unwrapRow (Row cs) = fmap unwrapCell cs
unwrapRow _ = [[]]

-- get markups embedded under audio and audio-insert elements
getAudioMarkups :: Markup -> [Markup]
getAudioMarkups (Audio _ _ ms) = ms
getAudioMarkups (AudioInsert _ ms) = ms
getAudioMarkups _ = []

-- get the attributes of audio elements
getAttrs :: Markup -> [Attr]
getAttrs (Audio attrs _ _) = attrs
getAttrs _ = []

-- audio insertion marker |> or 3|>
getInserts :: CSV -> [Markup]
getInserts (Cell ms) = filter isInsert ms
getInserts (Row cs) = concatMap getInserts cs
getInserts (Csv rs) = concatMap getInserts rs


isInsert :: Markup -> Bool
isInsert (AudioInsert _ _) = True
isInsert _ = False

isDefaultInsert :: Markup -> Bool
isDefaultInsert (AudioInsert 0 _) = True
isDefaultInsert _ = False

isEmptyInsert :: Markup -> Bool
isEmptyInsert (AudioInsert _ []) = True
isEmptyInsert _ = False

anyInsert :: CSV -> Bool
anyInsert (Cell ms) = any isInsert ms
anyInsert (Row cs) = foldr1 (||) (fmap anyInsert cs)
anyInsert (Csv rs) = foldr1 (||) (fmap anyInsert rs)

getInsertIndex :: Markup -> Maybe Int
getInsertIndex (AudioInsert i _) = Just i
getInsertIndex _ = Nothing

isDefaultAudio :: Markup -> Bool
isDefaultAudio (Audio _ 0 _) = True
isDefaultAudio _ = False

isEmptyAudio :: Markup -> Bool
isEmptyAudio (Audio _ _ []) = True
isEmptyAudio _ = False


-- clozes and their indices

getClozeIndex :: Markup -> Maybe Int
getClozeIndex (Cloze i _) = Just i
getClozeIndex _ = Nothing

isCloze :: Markup -> Bool
isCloze (Cloze _ _) = True
isCloze _ = False

isDefaultCloze :: Markup -> Bool
isDefaultCloze m = isCloze m && getClozeIndex m == Just 0

getClozes :: CSV -> [Markup]
getClozes (Cell ms) = filter isCloze ms
getClozes (Row cs) = concatMap getClozes cs
getClozes (Csv rs) = concatMap getClozes rs

-- gets audio elements for each CSV element: cell, row, and csv strings

getAudios :: CSV -> [Markup]
getAudios (Cell ms) = filter isAudio ms
getAudios (Row cs) = concatMap getAudios cs
getAudios (Csv rs) = concatMap getAudios rs

isAudio :: Markup -> Bool
isAudio (Audio _ _ _) = True
isAudio _ = False

anyAudios' :: [Markup] -> Bool
anyAudios' ms = any isAudio ms

anyAudios :: CSV -> Bool
anyAudios (Cell ms) = anyAudios' ms
anyAudios (Row cs) = foldr1 (||) (fmap anyAudios cs)
anyAudios (Csv rs) = foldr1 (||) (fmap anyAudios rs)

noAudios :: CSV -> Bool
noAudios = not . anyAudios

getAudioIndex :: Markup -> Maybe Int
getAudioIndex (Audio _ i _) = Just i
getAudioIndex _ = Nothing


-- general functions on markups

isIndexable :: Markup -> Bool
isIndexable (Audio _ _ _) = True
isIndexable (AudioInsert _ _) = True
isIndexable (Cloze _ _) = True
isIndexable _ = False

getIndex :: Markup -> Maybe Int
getIndex (Cloze i _) = Just i
getIndex (Audio _ i _) = Just i
getIndex (AudioInsert i _) = Just i
getIndex _ = Nothing

hasIndex :: Markup -> Int -> Bool
hasIndex m i = Just i == getIndex m

updateIndex :: Int -> Markup -> Markup
updateIndex i (Cloze _ str) = Cloze i str
updateIndex i (Audio as _ ms) = Audio as i ms
updateIndex i (AudioInsert _ ms) = AudioInsert i ms
updateIndex _ m = m

updateIndexWith :: Int -> (Markup -> Bool) -> Markup -> Markup
updateIndexWith i pred m = if pred m then updateIndex i m else m 


updateIndicesWithConst :: Int -> (Markup -> Bool) -> CSV -> CSV
updateIndicesWithConst i pred (Cell ms) = Cell $ updateIndicesWith' i pred ms 
updateIndicesWithConst i pred (Row cs) = Row $ updateIndicesWithConst i pred <$> cs
updateIndicesWithConst i pred (Csv rs) = Csv $ updateIndicesWithConst i pred <$> rs

updateIndicesWith' :: Int -> (Markup -> Bool) -> [Markup] -> [Markup]
updateIndicesWith' i pred ms = updateIndexWith i pred <$> ms

-- retrieves the maximal index in c and returns an index greater than it by n
newIndexWith :: CSV -> Int -> Int
newIndexWith c n = maximum (getIndicesWith c isIndexable) + n

getIndices :: CSV -> [Int]
getIndices c = getIndicesWith c isIndexable

getIndicesWith :: CSV -> (Markup -> Bool) -> [Int]
getIndicesWith c pred =
  [i | m <- flattenAudios c, pred m, Just i <- [getIndex m]]
-- NB: pred must be used to select *indexed* markups

getMarkups :: Markup -> [Markup]
getMarkups (AudioInsert _ ms) = ms
getMarkups (Audio _ _ ms) = ms
getMarkups m = [m]

isDefaultMarkup :: Markup -> Bool
isDefaultMarkup (Cloze 0 _) = True
isDefaultMarkup (Audio _ 0 _) = True
isDefaultMarkup (AudioInsert 0 _) = True
isDefaultMarkup _ = False

deleteWith :: (Markup -> Bool) -> Int -> [Markup] -> [Markup]
deleteWith _ _ [] = []
deleteWith pred i (m:ms) | pred m && getIndex m == Just i = ms
                         | otherwise                      = m : deleteWith pred i ms

-- for a CSV element, get its number of rows and its number of cells
numRowsAndCells :: CSV -> (Int, Int)
numRowsAndCells (Csv rs) = (length rs, sum $ fmap (snd . numRowsAndCells) rs)
numRowsAndCells (Row cs) = (1, length cs)
numRowsAndCells (Cell _) = (0, 1)

numRows :: CSV -> Int
numRows c = case getRows c of
  Nothing -> 0
  Just rs -> length rs

numCells :: CSV -> Int
numCells c = case getCells c of
  Nothing -> 0
  Just cs -> length cs

-- the number of markup elements in a CSV
lengthCSV :: CSV -> Int
lengthCSV (Csv rs) = sum $ fmap lengthCSV rs
lengthCSV (Row cs) = sum $ fmap lengthCSV cs
lengthCSV c@(Cell _) = length $ flattenAudios c
--NB: length (Audio _ _ ms) = length ms + 1


hasEmptyRows :: CSV -> Bool
hasEmptyRows (Csv rs) = any (== Row []) rs
hasEmptyRows _ = False

-- synonym for adding CSV elements
(<+>) :: CSV -> CSV -> CSV
(<+>) = addCSV

-- adds a CSV element to another
addCSV :: CSV -> CSV -> CSV
addCSV (Csv rs) (Csv rs') = Csv $ rs ++ rs'
addCSV csv@(Csv _) r@(Row _) = addRow csv r
addCSV r@(Row _) csv@(Csv _) = addRow r csv
addCSV c@(Cell _) x = addCell c x
addCSV x c@(Cell _) = addCell x c
addCSV (Row _) (Row _) = error "Cannot add two rows directly"

-- adds a row to another CSV element
addRow :: CSV -> CSV -> CSV
addRow (Csv rs) r@(Row _) = Csv $ rs ++ [r]
addRow r@(Row _) (Csv rs) = Csv $ r : rs
addRow r@(Row _) c@(Cell _) = addCell r c
addRow c@(Cell _) r@(Row _) = addCell r c
addRow (Row _) (Row _) = error "Cannot add two rows directly"
addRow (Csv _) (Cell _) = error "Cannot add cell to CSV directly"
addRow (Csv _) (Csv _) = error "Cannot add CSV to CSV via addRow"
addRow (Cell _) (Cell _) = error "Cannot add cell to cell via addRow"
addRow (Cell _) (Csv _) = error "Cannot add CSV to cell via addRow"

-- adds a cell to another CSV element
addCell :: CSV -> CSV -> CSV
addCell (Csv rs) c@(Cell _) | null rs   = Csv [Row [c]]
                            | otherwise =
                                case rs of
                                  (Row cs : rs') -> Csv $ Row (cs ++ [c]) : rs'
                                  _ -> error "Expected Row in CSV"
addCell c@(Cell _) (Csv rs) | null rs    = Csv [Row [c]]
                            | otherwise =
                                case rs of
                                  (Row cs : rs') -> Csv $ Row (c:cs) : rs'
                                  _ -> error "Expected Row in CSV"
addCell (Row cs) c@(Cell _) = Row $ cs ++ [c]
addCell c@(Cell _) (Row cs) = Row $ c:cs
addCell (Cell ms) (Cell ms') = Row [Cell (ms ++ ms')]
addCell (Csv _) (Row _) = error "Cannot add row to CSV via addCell"
addCell (Row _) (Row _) = error "Cannot add row to row via addCell"
addCell (Csv _) (Csv _) = error "Cannot add CSV to CSV via addCell"
addCell (Row _) (Csv _) = error "Cannot add CSV to row via addCell"
--NB: to add a cell, addCell may need to add a row if the CSV element hasn't got one already!

addToAudio :: Markup -> Markup -> Markup
addToAudio (Audio as i ms) m = Audio as i $ ms ++ [m] 
addToAudio m _ = m


-- empty CSV elements: namely, Cells and Rows 
isEmptyCSV :: CSV -> Bool
isEmptyCSV (Cell []) = True
isEmptyCSV (Row []) = True
isEmptyCSV _ = False

deleteEmptyCSV :: CSV -> CSV
deleteEmptyCSV (Csv rs) = 
  let rs' = filter (not . isEmptyCSV) rs
      delEmptyCells [] = []
      delEmptyCells (rw:rws) =
        case rw of
          Row cs -> Row (filter (not . isEmptyCSV) cs) : delEmptyCells rws
          _ -> error "Expected Row in CSV"
  in Csv $ delEmptyCells rs'
deleteEmptyCSV (Cell ms) = Cell ms
deleteEmptyCSV (Row cs) = Row $ filter (not . isEmptyCSV) cs

-- just for listing or counting all the markups
flattenAudios :: CSV -> [Markup]
flattenAudios (Csv rs) = concatMap flattenAudios rs
flattenAudios (Row cs) = concatMap flattenAudios cs
flattenAudios (Cell []) = []
flattenAudios (Cell (m:ms))
  | isAudio m = case m of
      Audio as i ms' -> (Audio as i [] : ms') ++ flattenAudios (Cell ms)
      _ -> error "Expected Audio markup"
  | otherwise = m : flattenAudios (Cell ms)


-- limit the size of a CSV element to #rows nr, #cells nc, #markups nm
takeCSV :: CSV -> Int -> Int -> Int -> CSV
takeCSV (Csv rs) nr nc nm = Csv $ take nr (fmap (\r -> takeCSV r nr nc nm) rs)   
takeCSV (Row cs) nr nc nm = Row $ take nc (fmap (\c -> takeCSV c nr nc nm) cs)
takeCSV (Cell ms) _ _ nm = Cell $ take nm ms

-- determin whether a CSV element is empty or has empty elements
hasEmptyElems :: CSV -> Bool
hasEmptyElems (Csv []) = True
hasEmptyElems (Csv rs) = any (== Row []) rs || or (fmap hasEmptyElems rs)
hasEmptyElems (Row []) = True
hasEmptyElems (Row cs) = any (== Cell []) cs
hasEmptyElems (Cell []) = True
hasEmptyElems (Cell (_:_)) = False

-- CSV predicates

isRow :: CSV -> Bool
isRow (Row _) = True
isRow _ = False

isCell :: CSV -> Bool
isCell (Cell _) = True 
isCell _ = False 

-- CSV filters

-- get a non-empty list of rows or nothing
getRows :: CSV -> Maybe [Row]
getRows (Csv rs) = if not (null rs) then Just rs else Nothing
getRows _ = Nothing

-- get a non-empty list of cells
getCells :: CSV -> Maybe [Cell]
getCells (Csv rs) = 
  let getCells' (Row cs) = cs 
      getCells' (Cell _) = []
      getCells' (Csv _) = []
      cs' = concatMap getCells' rs
  in if not (null cs') then Just cs' else Nothing 
getCells (Row cs) = if not (null cs) then Just cs else Nothing
getCells _ = Nothing

-- get row number n (row counting starts at 1)
getRowNum :: Int -> CSV -> Maybe Row
getRowNum n c =
  case getRows c of
    Just rs -> if n > length rs then Nothing else Just $ rs !! (n-1)
    _       -> Nothing

-- get cell number n (cell counting starts at 1)
getCellNum :: Int -> CSV -> Maybe Cell
getCellNum n c =
  case getCells c of
    Just cs -> if n > length cs then Nothing else Just $ cs !! (n-1)
    _       -> Nothing

-- get cell number nc in row number nr
getRowCell :: Int -> Int -> CSV -> Maybe Cell
getRowCell nr nc c = getRowNum nr c >>= getCellNum nc


-- simplify AST by emptying its markup elements; the AST structure becomes more clear

bareBones :: CSV -> CSV
bareBones (Cell ms) = Cell $ fmap bareBonesMarkup ms
bareBones (Row cs) = Row $ fmap bareBones cs
bareBones (Csv rs) = Csv $ fmap bareBones rs

bareBonesMarkup :: Markup -> Markup
bareBonesMarkup (Audio _ i _) = Audio [] i []
bareBonesMarkup (AudioInsert i _) = AudioInsert i []
bareBonesMarkup (Cloze i str) = Cloze i (take 4 str)
bareBonesMarkup (Bold str) = Bold $ take 4 str
bareBonesMarkup (Italic str) = Italic $ take 4 str
bareBonesMarkup (Regular str) = Regular $ take 4 str
bareBonesMarkup EndCell = EndCell
bareBonesMarkup EndRow = EndRow