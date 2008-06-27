-- ----------------------------------------------------------------------------

{- |
  Module     : DiffListTest
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the difference encoding of numbers.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-missing-signatures #-}

module DiffListTest (allTests, allProperties) where

import Data.List

import qualified Data.IntSet as IS

import qualified Holumbus.Data.DiffList as DL

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

tests :: [(String, [Int])]
tests = 
  [ ("Simple sequence", [1,2,3,4,5,6,7,8,9])
  , ("Simple sequence with zero", [0,1,2,3,4,5,6,7,8,9])
  , ("Very long sequence", [1..100000])
  , ("Big gaps", [10000,20000,30000,40000,50000,60000,70000])
  , ("Big and small gaps", [10000,10001,20000,20002,30000,30003,40000,40004])
  , ("Random numbers", [3454,9874,21,6187,384,13,984,617,42,313])
  ]

listTests :: Test
listTests = TestList $ map makeListTest tests
  where
  makeListTest (desc, values) = TestCase 
    (assertEqual desc (sort values) ((DL.toList . DL.fromList) values))
  
intSetTests :: Test
intSetTests = TestList $ map makeIntSetTest tests
  where
  makeIntSetTest (desc, values) = TestCase 
    (assertEqual desc (sort values) ((IS.toList . DL.toIntSet . DL.fromIntSet . IS.fromList) values))

valid :: (Ord a, Num a) => [a] -> Bool
valid [] = True
valid (x:xs) = (x <= 65535) && (x >= 0) && (valid xs)

prop_FromToList xs = valid xs ==> DL.toList (DL.fromList xs) == (sort xs)
prop_FromToIntSet xs = valid xs ==> DL.toIntSet (DL.fromIntSet (IS.fromList xs)) == (IS.fromList xs)
prop_EqListSet xs = valid xs ==> DL.toIntSet (DL.fromList $ nub xs) == IS.fromList xs
prop_EqSetList xs = valid xs ==> DL.toList (DL.fromIntSet (IS.fromList xs)) == (sort $ nub xs)

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("DiffList tests",
                [ run prop_FromToList
                , run prop_FromToIntSet
                , run prop_EqListSet
                , run prop_EqSetList
                ])

allTests :: Test  
allTests = TestLabel "DiffList tests" $ 
  TestList
  [ TestLabel "From/to List tests" listTests
  , TestLabel "From/to IntSet tests" intSetTests
  ]