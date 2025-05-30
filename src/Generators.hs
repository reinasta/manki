{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Generators where

import RIO hiding (many,some,try)
import Test.QuickCheck

import Types

-- QuickCheck generators

instance Show (Gen a) where
  show _ = "<generator>"

instance Arbitrary Attr where
  arbitrary = elements [Visible, Invisible]


-- Markup instance

instance Arbitrary Markup where
  arbitrary = do n <- choose (1,3) :: Gen Int
                 case n of
                   1 -> sized genAudio
                   2 -> genSubmarkup
                   3 -> sized genInsert
                   _ -> genSubmarkup



genAll :: Int -> Gen [Markup]
genAll m = do
  n <- choose (0, m `div` 2)
  subs <- sized genSubmarkups
  a <- sized genAudio
  i <- sized genInsert
  return $ take n (a : i : subs)

genAudio :: Int -> Gen Markup
genAudio m = do
  as <- vectorOf 1 arbitrary :: Gen [Attr]
  i <- arbitrary :: Gen Int
  n <- choose (0, m `div` 2)
  ms <- vectorOf n (genAudio (m `div` 4))
  return $ Audio as (abs i) ms


genInsert :: Int -> Gen Markup
genInsert m = do
  i <- arbitrary :: Gen Int
  n <- choose (0, m `div` 2)
  ms <- vectorOf n (genInsert (m `div` 4))
  ms' <- sized genSubmarkups
  return $ AudioInsert (abs i) (ms ++ ms')

genSubmarkups :: Int -> Gen [Markup]
genSubmarkups m = do
  n <- choose (0, m `div` 2)
  ms <- vectorOf n genSubmarkup
  return ms


genSubmarkup :: Gen Markup
genSubmarkup = do
  n <- choose (1,4) :: Gen Int
  case n of
    1 -> do str <- fmap onlySafeChars arbitrary
            i <- arbitrary
            return $ Cloze (abs i) str
    2 -> do str <- fmap onlySafeChars arbitrary
            return $ Bold str
    3 -> do str <- fmap onlySafeChars arbitrary
            return $ Italic str
    4 -> do str <- fmap onlySafeChars arbitrary
            return $ Regular str
    _ -> do str <- fmap onlySafeChars arbitrary
            return $ Regular str

-- as above, without the Cloze case
genSubmarkup' :: Gen Markup
genSubmarkup' = do
  n <- choose (1,3) :: Gen Int
  let shortStrGen = vectorOf 4 arbitraryASCIIChar
  case n of
    1 -> do str <- fmap onlySafeChars shortStrGen
            return $ Bold str
    2 -> do str <- fmap onlySafeChars shortStrGen
            return $ Italic str
    3 -> do str <- fmap onlySafeChars shortStrGen
            return $ Regular str
    _ -> do str <- fmap onlySafeChars shortStrGen
            return $ Regular str



-- makes sure the strings do not contain markers: *@_~
onlySafeChars :: String -> String
onlySafeChars str = filter nonmarker str
  where nonmarker c = c `notElem` "*@_~"


-- CSV instance

instance Arbitrary CSV where
  arbitrary = sized genCsv


genCell :: Int -> Gen CSV
genCell m = do
  n <- choose (0, m `div` 2)
  Cell <$> vectorOf n arbitrary

genRow :: Int -> Gen CSV
genRow m = do
  n <- choose (0, m `div` 4)
  Row <$> vectorOf n (sized genCell)

genCsv :: Int -> Gen CSV
genCsv m = do
  n <- choose (0, m `div` 4)
  Csv <$> vectorOf n (sized genRow)


-- get samples for testing

sample_markup :: IO [Markup]
sample_markup = sample' arbitrary :: IO [Markup]

gen_nonempty_csv :: Gen CSV
gen_nonempty_csv = arbitrary :: Gen CSV

sample_rows :: IO [Row]
sample_rows = sample' (sized genRow) :: IO [Row]

