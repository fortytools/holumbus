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

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-missing-methods -fno-warn-orphans #-}

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

instance Arbitrary Documents where
  arbitrary =
    do
    ids <- sized (\n -> sequence [ (choose (1, (n + 1))) | _ <- [0..n] ])
    docs <- liftM (nubBy (\(i1, _) (i2, _) -> i1 == i2)) (mapM (\d -> liftM2 (,) (return d) arbitrary) ids)
    i2d <- return (IM.fromList docs)
    d2i <- return (M.fromList (map (\(i, (_, u)) -> (u, i)) docs))
    l <- return (fst $ maximumBy (compare `on` fst) docs)
    return (Documents i2d d2i l)

prop_InsertLookupId d di = (snd d) /= ""
  ==> let (i, ds) = insertDoc (di :: Documents) d in
      lookupById ds i == Just d

prop_InsertLookupURI d di = (snd d) /= ""
  ==> let (i, ds) = insertDoc (di :: Documents) d in
      lookupByURI ds (snd d) == Just i

prop_InsertLastId d di = (snd d) /= ""
  ==> let (i, ds) = insertDoc (di :: Documents) d in
      (IM.foldWithKey (\k _ r -> max k r) 0 (idToDoc ds)) == i

prop_InsertLookupIdEmpty d = (snd d) /= ""
  ==> let (i, ds) = insertDoc emptyDocuments d in
      lookupById ds i == Just d

prop_InsertLookupURIEmpty d = (snd d) /= ""
  ==> let (i, ds) = insertDoc emptyDocuments d in
      lookupByURI ds (snd d) == Just i

prop_InsertSize d di = (snd d) /= ""
  ==> let (_, ds) = insertDoc (di :: Documents) d in
      sizeDocs ds == (sizeDocs di) + 1

prop_InsertSizeEmpty d = (snd d) /= ""
  ==> let (_, ds) = insertDoc emptyDocuments d in
      sizeDocs ds == (sizeDocs emptyDocuments) + 1

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("Documents tests",
                [ run prop_InsertLookupId
                , run prop_InsertLookupURI
                , run prop_InsertSize
                , run prop_InsertLookupIdEmpty
                , run prop_InsertLookupURIEmpty
                , run prop_InsertSizeEmpty
                , run prop_InsertLastId
                ])

allTests :: Test  
allTests = TestLabel "Documents tests" $ 
  TestList
  [
  ]
   
 