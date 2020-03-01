

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
updateIndicesWith c pred =
  let -- generate indices which do not overlap with existing ones
      generateOddIndices i = filter (>i) [1,3 ..] \\ nos :: [Int]
      nos = explicitIndicesCSV c :: [Int]
      explicitIndicesCSV :: CSV -> [Int]
      explicitIndicesCSV c' =
        let targets = filter pred (flattenAudios c')
            mis = sequence $ fmap getIndex targets
        in fromMaybe [] mis

      -- use indices 'is' to update a CSV element
      updateCSV :: [Int] -> CSV -> CSV
      updateCSV is c = case c of
        Cell ms -> Cell $ updateMarkups is ms
        Row cs -> Row $ updateCells is cs
        Csv rs -> Csv $ updateCSV is <$> rs

      updateCells is [] = []
      updateCells is (c:cs) =
        updateCell is c : updateCells (getNewIndices c is) cs
        where
          getNewIndices c' is' = drop (countDefaults' (unwrapCSV c') + 1) is'
          countDefaults' ms' = sum $ countDefaults <$> ms'

      updateCell :: [Int] -> Cell -> Cell
      updateCell is (Cell ms) = Cell $ updateMarkups is ms

      -- update markup lists
      updateMarkups :: [Int] -> [Markup] -> [Markup]
      updateMarkups is [] = []
      updateMarkups inds@(i:is) (m:ms) =
        updateDefault i m : updateMarkups (drop (countDefaults m) inds) ms

      -- updates default markup indices, including of markups embedded under Audio
      updateDefault i (Audio as k ms) = Audio as (if k == 0 then i else k) $
        case ms of
          []      -> []
          _       -> let updateEmbedded' [] j = []
                         updateEmbedded' (m':ms') j =
                           if m' `isDefault` pred
                             then updateIndex (head $ generateOddIndices j) m' :
                                    updateEmbedded' ms' (j+2)
                             else m' : updateEmbedded' ms' j
                     in updateEmbedded' ms i

      updateDefault i m = updateDefaultIndexWith pred i m


      countDefaults :: Markup -> Int
      countDefaults m =
        let lengthEmbeddedDefaults ms' =
              length $ filter (\mrk -> mrk `isDefault` pred) ms'
        in case m of -- in Audio we count twice as embedded indices take only odd values
             Audio _ i ms -> 2 * lengthEmbeddedDefaults ms + (if i == 0 then 1 else 0)
             _            -> if m `isDefault` pred then 1 else 0

      -- m is a default target markup if it satisfies p (e.g. isCloze) and has index 0
      isDefault m p = p m && m `hasIndex` 0
      -- update the default index of a target markup (identified using the predicate p)
      updateDefaultIndexWith p i m = if m `isDefault` p then updateIndex i m else second i m
        where second a1 a2 = a2

  in updateCSV ([1..] \\ nos) c


rewriteIndices :: CSV -> CSV
rewriteIndices = rewriteInsertIndices . rewriteAudioIndices . rewriteClozeIndices

rewriteClozeIndices, rewriteAudioIndices, rewriteInsertIndices :: CSV -> CSV
rewriteClozeIndices c = c `rewriteIndicesWith` isCloze
rewriteAudioIndices c = c `rewriteIndicesWith` isAudio
rewriteInsertIndices c = c `rewriteIndicesWith` isInsert

--general version: rewrite indices
rewriteIndicesWith :: CSV -> (Markup -> Bool) -> CSV
rewriteIndicesWith c pred =
  let -- general update: replace index 'old' with index 'new' on markups in csv element
      update :: Int -> Int -> CSV -> CSV
      update old new c = case c of
        Cell ms -> Cell $ updateMarkups old new ms
        Row cs -> Row $ update old new <$> cs
        Csv rs -> Csv $ update old new <$> rs
        where -- update markups, including those under Audio elements
          updateMarkup old new m
            | isAudio m = let Audio as i ms' = m
                              updateAudio m' = if pred m' && old == i then updateIndex new m' else m'
                          in updateAudio $ Audio as i $ updateMarkup old new <$> ms'
            | pred m && getIndex m == Just old = updateIndex new m -- depends on PRED
            | otherwise = m

          updateMarkups old new ms = [updateMarkup old new m | m <- ms]

      -- the list of old indices we want to replace -- depends on PRED
      olds = nub [let Just i = getIndex m in i | m <- flattenAudios c, pred m]
      -- the list of indices that we will eventually use to update the old ones
      news = [1 .. computeMaxAllowableIndex c]
        where computeMaxAllowableIndex :: CSV -> Int
              computeMaxAllowableIndex c =
                let ms' = flattenAudios c -- code below depends on PRED
                    targetIndices = fromMaybe [] $ sequence $ getIndex <$> filter pred ms'
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

