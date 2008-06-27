-- ----------------------------------------------------------------------------

{- |
  Module     : InvertedTest
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Some tests for the inverted index.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS 
    -fno-warn-orphans 
    -fno-warn-missing-signatures 
    -fno-warn-missing-methods 
    -fno-warn-type-defaults
#-}

module InvertedTest (allTests, allProperties) where

import Control.Monad

import Data.Char
import Data.List

import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import qualified Holumbus.Data.StrMap as SM
import qualified Holumbus.Data.DiffList as DL

import Holumbus.Index.Common
import Holumbus.Index.Inverted

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

instance Arbitrary Char where
  arbitrary     = choose ('\32', '\128')
  coarbitrary c = variant (ord c `rem` 4)

instance Arbitrary Inverted where
  arbitrary = liftM createInverted genParts

instance Arbitrary a => Arbitrary (IM.IntMap a) where
  arbitrary =
    do
    ds <- sized (\n -> choose (1, n + 1) >>= vector)
    os <- mapM (\d -> liftM2 (,) (return d) arbitrary) ds
    return (IM.fromList os)

instance Arbitrary IS.IntSet where
  arbitrary =
    do
    ps <- sized (\n -> (choose (1, n + 1) >>= vector))
    return (IS.fromList (map (\x -> abs (if x > 65535 then 65535 else x)) ps))

genWord :: Gen [Char]
genWord = sequence [ arbitrary | _ <- [1..20] ]

genPart :: Gen [(String, Occurrences)]
genPart = sized (\n -> sequence [ (liftM2 (,) genWord arbitrary) | _ <- [1..n + 1] ])

genParts :: Gen [(String, [(String, Occurrences)])]
genParts = sequence [ (liftM2 (,) genWord genPart) | _ <- [1..5] ]

deflate' = IM.map DL.fromIntSet

createInverted :: [(String, [(String, Occurrences)])] -> Inverted
createInverted x = Inverted (parts x)
  where
  parts = M.fromList . (map (\(c, p) -> (c, SM.fromList (map (\(w, o) -> (w, deflate' o)) p))))

prop_SingletonInsert c w o = c /= "" && w /= ""
  ==> singleton c w o == insertOccurrences c w o emptyInverted

prop_InsertMerge c w o i = (c /= "") && (w /= "")
  ==> insertOccurrences c w o i == mergeIndexes (singleton c w o) i
  
prop_LookupInsert c w o i = (c /= "") && (w /= "")  
  ==> lookupCase (insertOccurrences c w o i :: Inverted) c w == [(w, o)]

prop_LookupMerge c w o i = (c /= "") && (w /= "")
  ==> lookupCase (mergeIndexes (singleton c w o) i) c w == [(w, o)]

prop_PrefixInsert c w o i = (c /= "") && (w /= "")
  ==> (w, o) `elem` (prefixCase (insertOccurrences c w o i :: Inverted) c w)

prop_PrefixMerge c w o i = (c /= "") && (w /= "")
  ==> (w, o) `elem` (prefixCase (mergeIndexes (singleton c w o) i) c w)

prop_SizeSingleton c w o = 
  sizeWords (singleton c w o) == 1

prop_SizeAllWords i =
  foldr (\c r -> r + (length $ allWords i c)) 0 (contexts i) == sizeWords (i :: Inverted)

prop_SizeMerge i1 i2 =
  (sizeWords i1 + sizeWords i2) >= (sizeWords $ mergeIndexes i1 (i2 :: Inverted))

prop_ContextsInsert c w o i = (c /= "") && (w /= "")
  ==> c `elem` (contexts (insertOccurrences c w o i :: Inverted))

prop_ContextsMerge c w o i = (c /= "") && (w /= "")
  ==> c `elem` (contexts (mergeIndexes (singleton c w o) i))

-- FIXME TBH 07.03.2008: Does not work due to awkward generated indices.
--prop_SplitDocuments i =
--  let [i1, i2] = trace (show $ splitByDocuments i 2) $ splitByDocuments i 2 in mergeIndexes i1 i2 == (i :: Inverted)

prop_SplitWords i =
  let [i1, i2] = splitByWords i 2 in mergeIndexes i1 i2 == (i :: Inverted)

prop_SplitContexts i =
  let [i1, i2] = splitByContexts i 2 in mergeIndexes i1 i2 == (i :: Inverted)

prop_Substract i1 i2 = let merged = mergeIndexes i1 i2 :: Inverted in
  (substractIndexes merged i1 == i2)
  &&
  (substractIndexes merged i2 == i1)

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("Inverted tests",
                [ run prop_SingletonInsert
                , run prop_InsertMerge
                , run prop_LookupInsert
                , run prop_LookupMerge
                , run prop_PrefixInsert
                , run prop_PrefixMerge
                , run prop_SizeAllWords
                , run prop_SizeSingleton
                , run prop_SizeMerge
                , run prop_ContextsInsert
                , run prop_ContextsMerge
                , run prop_SplitWords
                , run prop_SplitContexts
                , run prop_Substract
                ])

allTests :: Test  
allTests = TestLabel "Inverted tests" $ 
  TestList
  [
  ]
   
 