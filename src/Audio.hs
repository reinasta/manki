{-# LANGUAGE NoImplicitPrelude #-}
module Audio where

import Types
import Util

import Prelude hiding (pred)
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import RIO.List (intersect,headMaybe,sortOn,groupBy,nubBy,insertBy,deleteBy,(\\),nub,find)


-- delete empty audios and audio inserts
removeEmptyAudios :: CSV -> CSV
removeEmptyAudios (Cell ms) = Cell $ filter (\m -> not $ isEmptyInsert m || isEmptyAudio m) ms
removeEmptyAudios (Row cs) = Row $ fmap removeEmptyAudios cs
removeEmptyAudios (Csv rs) = Csv $ fmap removeEmptyAudios rs
-- NB: none of the version of copyAudios deletes empty Audios or Inserts.
-- Apply this filter last, so as not to mess up the insertion of audio markups.
-- NB: that's not exactly true, as copyAudio removes empty insert duplicates


-- move markup contents between coindexed Audio elements and AudioInsert elements
copyAudios c = copyAudiosWith c (\_ -> True)
-- move markup contents between default (zero-indexed) audio and insert elements
copyAudiosDefault c = copyAudiosWith c isDefaultMarkup
-- move markup contents between non-default (index > 0) audio and insert elements
copyAudiosCustom c = copyAudiosWith c (not . isDefaultMarkup)

-- copies (unmerged) coindexed to different places, then deals with non-defaults
copyUnmergedAudios c = copyUnmergedAudiosWith customCsv isDefaultMarkup
  where
    customCsv = copyUnmergedAudiosWith c (not . isDefaultMarkup)

-- get audios from their original places in the CSV element and copy them
-- in the places marked with insert markers (effectively replacing the markers)
copyAudiosWith :: CSV -> (Markup -> Bool) -> CSV
copyAudiosWith c pred = copyModAudiosWith c pred mergeCoindexedAudios
-- NB: merges coindexed audios before attempting to move their markup contents

-- leaves coindexed audios unmerged, so they can be inserted at different places
copyUnmergedAudiosWith :: CSV -> (Markup -> Bool) -> CSV
copyUnmergedAudiosWith c pred = copyModAudiosOnceWith c pred id

{- IDEA: use the presence of a couple of default audios and inserts to trigger
a call to copyUnmergedAudiosWith, since this is likely the only use case for it -}

-- allows one to filter audios before copying them; use the 'mod' filter
copyModAudiosWith :: CSV -> (Markup -> Bool) -> ([Markup] -> [Markup]) -> CSV
copyModAudiosWith c pred mod =
  let -- extract audios from the first argument (ms1) and copy them in the second (ms2)
      getAndCopyAudiosWith ::  (Markup -> Bool) -> [Markup] -> [Markup] -> [Markup]
      getAndCopyAudiosWith p ms1 ms2 =
        let audios = mod $ filter (\m -> p m && isAudio m) ms1
        in placeMultipleAudiosWith p audios ms2
      -- a cell version of the above
      copyAudiosInCellWith :: [Markup] -> (Markup -> Bool) -> Cell -> Cell
      copyAudiosInCellWith ms p (Cell ms') = Cell $ getAndCopyAudiosWith p ms ms'
      copyAudiosInCellWith _ _ c = c

  in case c of
       (Cell ms) -> Cell $ getAndCopyAudiosWith pred ms ms
       (Csv rs) -> Csv $ (\r -> copyModAudiosWith r pred mod) <$> rs
       (Row cs) -> let rowLevelAudios = mod $ concatMap getAudios cs :: [Markup]
                   in Row $ copyAudiosInCellWith rowLevelAudios pred <$> cs

-- NB: the replacement of insert-markers with audios occurs primarily at
-- row level: all the audios in a row are placed in their respective places
-- in the cells of the row. Hence the more convoluted Row definition.

-- allows one to filter audios before copying them; use the 'mod' filter
copyModAudiosOnceWith :: CSV -> (Markup -> Bool) -> ([Markup] -> [Markup]) -> CSV
copyModAudiosOnceWith c pred mod =
  let -- extract audios from the first argument (ms1) and copy them into the second (ms2)
      getAndCopyAudiosWith ::  (Markup -> Bool) -> [Markup] -> [Markup] -> [Markup]
      getAndCopyAudiosWith p ms1 ms2 =
        let audios = mod $ filter (\m -> p m && isAudio m) ms1
        in placeMultipleAudiosOnceWith p audios ms2
      -- a cell version of the above
      copyAudiosInCellWith :: [Markup] -> (Markup -> Bool) -> Cell -> Cell
      copyAudiosInCellWith ms p (Cell ms') = Cell $ getAndCopyAudiosWith p ms ms'
      copyAudiosInCellWith _ _ c' = c'

  in case c of
       (Cell ms) -> Cell $ getAndCopyAudiosWith pred ms ms
       (Csv rs) -> Csv $ (\r -> copyModAudiosOnceWith r pred mod) <$> rs
       (Row cs) -> let rowLevelAudios = mod $ concatMap getAudios cs :: [Markup]
                   in Row $ placeMultipleAudiosOnceInCellsWith pred rowLevelAudios cs


-- moves multiple audios at their insert places in a list, not allowing default duplicates
placeMultipleAudiosWith :: (Markup -> Bool) -> [Markup] -> [Markup] -> [Markup]
placeMultipleAudiosWith _ [] ms = ms
placeMultipleAudiosWith _ _ [] = []
placeMultipleAudiosWith p (a:as) ms =
  placeMultipleAudiosWith p as $ placeAudioWith p a ms

-- versions of placeMultipleAudios for copying several default audios in several
-- (coindexed) default insert places

-- moves multiple audios at their insert places in a list, allowing default duplicates
placeMultipleAudiosOnceWith :: (Markup -> Bool) -> [Markup] -> [Markup] -> [Markup]
placeMultipleAudiosOnceWith _ [] ms = ms
placeMultipleAudiosOnceWith _ as [] = []
placeMultipleAudiosOnceWith p (a:as) ms =
  placeMultipleAudiosOnceWith p as $ placeAudioOnceWith p a ms

placeMultipleAudiosOnceInCellsWith :: (Markup -> Bool) -> [Markup] -> [Cell] -> [Cell]
placeMultipleAudiosOnceInCellsWith p as cs =
  fmap Cell $ placeMultipleAudiosOnceInListsWith p as $ fmap unwrapCSV cs

-- moves multiple audios at their insert places in multiple lists, allowing default duplicates
placeMultipleAudiosOnceInListsWith :: (Markup -> Bool) -> [Markup] -> [[Markup]] -> [[Markup]]
placeMultipleAudiosOnceInListsWith _ [] mss = mss
placeMultipleAudiosOnceInListsWith _ as [] = []
placeMultipleAudiosOnceInListsWith p as (ms:mss)
  | numReplacements as ms > 0 =
      placeMultipleAudiosOnceWith p as ms :
        placeMultipleAudiosOnceInListsWith p (as \\ audiosFitForInsertion as ms) mss
  | otherwise                = ms : placeMultipleAudiosOnceInListsWith p as mss
  where
    -- out of ads find the audios ready to be inserted in the markup list mks
    audiosFitForInsertion [] _ = []
    audiosFitForInsertion _ [] = []
    audiosFitForInsertion ads mks =
      let allas = filter isAudio ads
          allis = filter isInsert mks
          -- indices of the audios coindexed with the inserts in the second argument (mks)
          firstMatchingAudioIndices =
            [let Just idx = getIndex i in idx | i <- allis, any (audioInsertEq i) allas]
          -- collect the first audios matching indices in the first argument (is)
          keepFirstMatching [] as = []
          keepFirstMatching (i:is) as =
            let mfound = find (`hasIndex` i) allas
            in case mfound of
                 Nothing -> keepFirstMatching is as
                 Just found -> found : keepFirstMatching is (deleteBy audioInsertEq found as)
         -- get the first audios from the 2nd arg list with indices in the 2nd arg list
      in keepFirstMatching firstMatchingAudioIndices allas
    -- how many re-placements of audio contents (from ads) can be made into markups mks
    numReplacements ads mks = length $ audiosFitForInsertion ads mks
{- NB
   If the markups include: A0 [c1] ... A0 [c2] ... A [c3] ... I0 [] ... I0 [] ... I0 [], the
   contents c get copied as in: A0 [c1] ... A0 [c2] ... A [c3] ... I0 [c1] ... I0 [c2] ... I0 [c3].
   The A0s and I0s are not bound to the sample structure above and can appear in any order
   whatsoever, e.g. I0 A0 A0 I0 A0 I0. The leftmost I0 inherits content from the leftmost A0;
   the second I0 from the left inherits content from the second A0 from the left; and so on.
-}


-- moves markup-content irrespective of how their original Audio host is indexed
placeAudio :: Markup -> [Markup] -> [Markup]
placeAudio = placeAudioWith (\_ -> True)
-- moves only markups under a default (zero-indexed) Audio element
placeAudioDefault :: Markup -> [Markup] -> [Markup]
placeAudioDefault = placeAudioWith (isDefaultMarkup)
-- moves only markups under custom (nonzero-indexed) Audio element
placeAudioCustom :: Markup -> [Markup] -> [Markup]
placeAudioCustom = placeAudioWith (not . isDefaultMarkup)

-- substitutes an audio element for the *first* coindexed insertion marker
-- and removes any other empty insertion markers
placeAudioWith :: (Markup -> Bool) -> Markup -> [Markup] -> [Markup]
placeAudioWith p a ms =
  -- the two elements satisfy pred p (e.g. not . isDefaultMarkup)
  placeAudioManyTimesWith maxReplacements p a (noInsertDuplicates ms)
  -- NB: maxReplacements does not make any difference, as empty inserts are deleted!
  where
    -- number of replacements is set to the maximum
    maxReplacements = length $ filter (\m -> p m && audioInsertEq a m) ms
    -- remove duplicate audio-insert markers, keeping the first in each equality group
    noInsertDuplicates :: [Markup] -> [Markup]
    noInsertDuplicates ms = nubBy eqCustomInsertIndices ms
      where
        eqCustomInsertIndices (AudioInsert i []) (AudioInsert j []) = i == j
        eqCustomInsertIndices _ _ = False
-- NB: this version does not allow an audio to be placed at multiple locations
-- nor does it allow several default audios to be placed at different locations

-- substitutes an audio element for *one* empty coindexed insertion
-- marker if the two elements satisfy pred (e.g. not . isDefaultMarkup)
placeAudioOnceWith :: (Markup -> Bool) -> Markup -> [Markup] -> [Markup]
placeAudioOnceWith p a ms = placeAudioManyTimesWith 1 p a ms
-- NB: this version allows an audio to be placed in several locations
-- the other version removes insert duplicates before moving audios



-- places markups under Audio 'a' into 'n' different AudioInsert (if such exist in list 'ms')
placeAudioManyTimesWith :: Int -> (Markup -> Bool) -> Markup -> [Markup] -> [Markup]
placeAudioManyTimesWith n pred a ms = replaceWith pred n a ms
  where
    -- replace x in ms' n times
    replaceWith p n' x ms'
      | n' >= maxReplacements = map (shiftWhenCoindexedWith p x) ms'
      | otherwise             = iterate (replaceOnceWith p x) ms' !! n'
      where
        -- how many replacements can be made?
        maxReplacements = length $ filter (audioInsertEqWith p a) ms'
        -- replace x in ys just once
        replaceOnceWith _ _ [] = []
        replaceOnceWith p' x' (y:ys)
          | audioInsertEqWith p' x' y  = shiftWhenCoindexedWith p' x' y : ys
          | otherwise                  = y : replaceOnceWith p' x' ys

    -- coindexation for insert elements are concerned
    audioInsertEqWith :: (Markup -> Bool) -> Markup -> Markup -> Bool
    audioInsertEqWith p x' y' = audioInsertEq x' y' && p x'
    -- NB: p is meant to represent a property of indices of markup elements

    -- shifts markups from Audio to AudioInsert when the two elements are coindexed
    shiftWhenCoindexedWith :: (Markup -> Bool) -> Markup -> Markup -> Markup
    shiftWhenCoindexedWith p a' m = if audioInsertEqWith p a' m then a' `passOnMarkups` m else m
      where -- we pass markups only onto empty inserts (due to audioInsertEq)
        passOnMarkups (Audio _ _ ms') (AudioInsert n' _) = AudioInsert n' ms'
        passOnMarkups _ m' = m'

-- defines co-indexation (equality of indices) for audio and insert elements
audioInsertEq :: Markup -> Markup -> Bool
audioInsertEq (AudioInsert i []) (Audio _ j _) = i == j
audioInsertEq (Audio _ i _) (AudioInsert j []) = i == j
audioInsertEq _ _ = False
-- NB: the pattern matching on AudioInsert i [] amounts to an *emptiness* condition


-- operating on audios via their indices

-- merges coindexed audios; note that the original order of the markups is not preserved
mergeCoindexedAudios :: [Markup] -> [Markup]
mergeCoindexedAudios ms =
  fromMaybe [] $ sequence $ joinAudios <$> sortGroupAudios ms
  where
    -- sort audio markup on indices; NB: ms must contain only audios!
    sortGroupAudios :: [Markup] -> [[Markup]]
    sortGroupAudios ms' = sortGroupWith getAudioIndex ms'

    -- sorts and groups according to index (an index accessor is passed as argument)
    sortGroupWith :: (Markup -> Maybe Int) -> [Markup] -> [[Markup]]
    sortGroupWith getMaybeIndex ms' =
      let index m = let Just n = getMaybeIndex m in n
          eqIndices x y = index x == index y
      in groupBy eqIndices $ sortOn index ms'

    -- join a list of audio elements, taking the index of the first audio element
    joinAudios :: [Markup] -> Maybe Markup
    joinAudios ms =
      let mi = join $ getAudioIndex <$> headMaybe ms
          mms = fmap unwrapAudioElems ms
          attrs = nub $ concatMap getAttrs ms
      in case mi of
           Just i -> (Audio attrs i . concat) <$> sequence mms
           _      -> Nothing



