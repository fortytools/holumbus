-- ----------------------------------------------------------------------------

{- |
  Module     : DocumentsTest
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Some tests for the documents table.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS 
    -fno-warn-missing-signatures 
    -fno-warn-missing-methods 
    -fno-warn-orphans 
    -fno-warn-type-defaults
#-}

module DocumentsTest (allTests, allProperties) where

import Control.Monad

import Data.Char
import Data.List
import Data.Function

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Holumbus.Index.Common
import Holumbus.Index.Documents

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

instance Arbitrary Char where
  arbitrary     = choose ('\32', '\128')
  coarbitrary c = variant (ord c `rem` 4)

instance Arbitrary (Document a) where
  arbitrary = liftM3 Document genNonEmptyString genNonEmptyString (return Nothing)

instance Arbitrary (Documents a) where
  arbitrary =
    do
    ids <- sized (\n -> sequence [ (choose (1, (n + 1))) | _ <- [0..n] ])
    docs <- liftM (nubBy (\(i1, _) (i2, _) -> i1 == i2)) (mapM (\d -> liftM2 (,) (return d) arbitrary) ids)
    i2d <- return (IM.fromList docs)
    d2i <- return (M.fromList (map (\(i, d) -> (uri d, i)) docs))
    l <- return (fst $ maximumBy (compare `on` fst) docs)
    return (Documents i2d d2i l)

genNonEmptyString :: Gen [Char]
genNonEmptyString = sequence [ arbitrary | _ <- [1..20] ]

prop_InsertLookupId d di = (uri d) /= ""
  ==> let (i, ds) = insertDoc (di :: Documents Int) d in
      lookupById ds i == Just d

prop_InsertLookupURI d di = (uri d) /= ""
  ==> let (i, ds) = insertDoc (di :: Documents Int) d in
      lookupByURI ds (uri d) == Just i

prop_InsertLastId d di = (uri d) /= ""
  ==> let (i, ds) = insertDoc (di :: Documents Int) d in
      (IM.foldWithKey (\k _ r -> max k r) 0 (idToDoc ds)) == i

prop_InsertLookupIdEmpty d = (uri d) /= ""
  ==> let (i, ds) = insertDoc emptyDocuments (d :: Document Int) in
      lookupById ds i == Just d

prop_InsertLookupURIEmpty d = (uri d) /= ""
  ==> let (i, ds) = insertDoc emptyDocuments (d :: Document Int) in
      lookupByURI ds (uri d) == Just i

prop_InsertSize d di = (uri d) /= ""
  ==> let (_, ds) = insertDoc (di :: Documents Int) d in
      sizeDocs ds == (sizeDocs di) + 1

prop_InsertSizeEmpty d = (uri d) /= ""
  ==> let (_, ds) = insertDoc emptyDocuments (d :: Document Int) in
      sizeDocs ds == (sizeDocs (emptyDocuments :: Documents Int)) + 1

prop_MergeEmpty ds = (snd $ mergeDocs emptyDocuments ds) == ((snd $ mergeDocs ds emptyDocuments) :: Documents Int)

prop_MergeSize d ds = lookupByURI (ds :: Documents Int) (uri d) == Nothing
  ==> let dn = singleton d in (sizeDocs $ snd $ mergeDocs ds dn) == (sizeDocs $ snd $ mergeDocs dn ds)

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("Documents tests",
                [ run prop_InsertLookupId
                , run prop_InsertLookupURI
                , run prop_InsertSize
                , run prop_InsertLookupIdEmpty
                , run prop_InsertLookupURIEmpty
                , run prop_InsertSizeEmpty
                , run prop_InsertLastId
                , run prop_MergeEmpty
                , run prop_MergeSize
                ])

allTests :: Test  
allTests = TestLabel "Documents tests" $ 
  TestList
  [
  ]
   
 