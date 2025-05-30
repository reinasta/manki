

module Indices where


import Types
import Util


import Data.Maybe (fromMaybe)
import RIO.List ((\\),group,sort,nub)

-- update default indices with a new (constant) index
updateDefIndsConst :: CSV -> CSV
updateDefIndsConst c = updateIndicesWithConst (newIndexWith c 1) isDefaultMarkup c
--NB: preserves coindexation of audios and inserts, and of any other coindexed elements

-- a more descriptive name for the index update
updateDefIndsAscend :: CSV -> CSV
updateDefIndsAscend = updateIndices

-- update default indices in ascending (but non-incremental) order,
-- i.e. 1,3,4,7 .. rather than 1,2,3,4 .. etc
updateIndices :: CSV -> CSV
updateIndices = updateInsertIndices . updateAudioIndices . updateClozeIndices
-- NB: it preserves co-indexation within 'cloze' and 'audio' categories,
-- but also between audio and insert elements

updateClozeIndices, updateAudioIndices, updateInsertIndices :: CSV -> CSV
updateClozeIndices c = c `updateIndicesWith` isCloze
updateAudioIndices c = c `updateIndicesWith` isAudio
updateInsertIndices c = c `updateIndicesWith` isInsert

-- update only the the default indices
updateIndicesWith :: CSV -> (Markup -> Bool) -> CSV
updateIndicesWith c predicate =
  let -- generate indices which do not overlap with existing ones
      generateOddIndices i = filter (>i) [1,3 ..] \\ nos :: [Int]
      nos = explicitIndicesCSV c :: [Int]
      explicitIndicesCSV :: CSV -> [Int]
      explicitIndicesCSV c' =
        let targets = filter predicate (flattenAudios c')
            mis = sequence $ fmap getIndex targets
        in fromMaybe [] mis

      -- use indices 'is' to update a CSV element
      updateCSV :: [Int] -> CSV -> CSV
      updateCSV is csvElem = case csvElem of
        Cell ms -> Cell $ updateMarkups is ms
        Row cs -> Row $ updateCells is cs
        Csv rs -> Csv $ updateCSV is <$> rs

      updateCells _ [] = []
      updateCells is (cellElem:cs) =
        updateCell is cellElem : updateCells (getNewIndices cellElem is) cs
        where
          getNewIndices c' is' = drop (countDefaults' (unwrapCSV c') + 1) is'
          countDefaults' ms' = sum $ countDefaults <$> ms'

      updateCell :: [Int] -> Cell -> Cell
      updateCell is (Cell ms) = Cell $ updateMarkups is ms
      updateCell _ (Row _) = error "Expected Cell, got Row"
      updateCell _ (Csv _) = error "Expected Cell, got Csv"

      -- update markup lists
      updateMarkups :: [Int] -> [Markup] -> [Markup]
      updateMarkups _ [] = []
      updateMarkups [] (_:_) = error "Not enough indices for markups"
      updateMarkups inds@(i:_) (m:ms) =
        updateDefault i m : updateMarkups (drop (countDefaults m) inds) ms

      -- updates default markup indices, including of markups embedded under Audio
      updateDefault i (Audio as k ms) = Audio as (if k == 0 then i else k) $
        case ms of
          []      -> []
          _       -> let updateEmbedded' [] _ = []
                         updateEmbedded' (m':ms') j =
                           if m' `isDefault` predicate
                             then case generateOddIndices j of
                                    (idx:_) -> updateIndex idx m' : updateEmbedded' ms' (j+2)
                                    []      -> m' : updateEmbedded' ms' j
                             else m' : updateEmbedded' ms' j
                     in updateEmbedded' ms i

      updateDefault i m = updateDefaultIndexWith predicate i m


      countDefaults :: Markup -> Int
      countDefaults m =
        let lengthEmbeddedDefaults ms' =
              length $ filter (\mrk -> mrk `isDefault` predicate) ms'
        in case m of -- in Audio we count twice as embedded indices take only odd values
             Audio _ i ms -> 2 * lengthEmbeddedDefaults ms + (if i == 0 then 1 else 0)
             _            -> if m `isDefault` predicate then 1 else 0

      -- m is a default target markup if it satisfies p (e.g. isCloze) and has index 0
      isDefault m p = p m && m `hasIndex` 0
      -- update the default index of a target markup (identified using the predicate p)
      updateDefaultIndexWith p i m = if m `isDefault` p then updateIndex i m else m

  in updateCSV ([1..] \\ nos) c


rewriteIndices :: CSV -> CSV
rewriteIndices = rewriteInsertIndices . rewriteAudioIndices . rewriteClozeIndices

rewriteClozeIndices, rewriteAudioIndices, rewriteInsertIndices :: CSV -> CSV
rewriteClozeIndices c = c `rewriteIndicesWith` isCloze
rewriteAudioIndices c = c `rewriteIndicesWith` isAudio
rewriteInsertIndices c = c `rewriteIndicesWith` isInsert

--general version: rewrite indices
rewriteIndicesWith :: CSV -> (Markup -> Bool) -> CSV
rewriteIndicesWith c predicate =
  let -- general update: replace index 'old' with index 'new' on markups in csv element
      update :: Int -> Int -> CSV -> CSV
      update oldIdx newIdx csvElem = case csvElem of
        Cell ms -> Cell $ updateMarkups oldIdx newIdx ms
        Row cs -> Row $ update oldIdx newIdx <$> cs
        Csv rs -> Csv $ update oldIdx newIdx <$> rs
        where -- update markups, including those under Audio elements
          updateMarkup oldVal newVal m
            | isAudio m = case m of
                Audio as i ms' -> 
                  let updateAudio m' = if predicate m' && oldVal == i then updateIndex newVal m' else m'
                  in updateAudio $ Audio as i $ updateMarkup oldVal newVal <$> ms'
                _ -> m -- This case should not happen due to isAudio guard
            | predicate m && getIndex m == Just oldVal = updateIndex newVal m -- depends on PREDICATE
            | otherwise = m

          updateMarkups oldVal newVal ms = [updateMarkup oldVal newVal m | m <- ms]

      -- the list of old indices we want to replace -- depends on PRED
      olds = nub [i | m <- flattenAudios c, predicate m, Just i <- [getIndex m]]
      -- the list of indices that we will eventually use to update the old ones
      news = [1 .. computeMaxAllowableIndex c]
        where computeMaxAllowableIndex :: CSV -> Int
              computeMaxAllowableIndex csvData =
                let ms' = flattenAudios csvData -- code below depends on PRED
                    targetIndices = fromMaybe [] $ sequence $ getIndex <$> filter predicate ms'
                in length $ filter (not . null) $ group $ sort targetIndices

      -- a index list that does not intersect with the old indices or the new ones
      nonintersects = [maxOrZero olds + 1 .. ]
        where maxOrZero xs = if null xs then 0 else maximum xs

      -- take the old indices one by one and replace them with new ones (drawn from an ascending list)
      rewrite rw [] _ = rw
      rewrite rw (o:os) (n:ns) = rewrite (update o n rw) os ns
      rewrite rw _ _ = rw

     -- update first with an intermediary (non-intersecting) list and then with the desired list
  in rewrite (rewrite c olds nonintersects) nonintersects news

