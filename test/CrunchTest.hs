-- ----------------------------------------------------------------------------

{- |
  Module     : CrunchTest
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Some tests for the crunching facility.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module CrunchTest (allTests, allProperties) where

import Holumbus.Data.Crunch

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

valid64 :: (Ord a, Num a) => [a] -> Bool
valid64 [] = True
valid64 (x:xs) = (x <= 2 ^ 60 - 1) && (x >= 0) && (valid64 xs)

valid32 :: (Ord a, Num a) => [a] -> Bool
valid32 [] = True
valid32 (x:xs) = (x <= 2 ^ 32 - 1) && (x >= 0) && (valid32 xs)

valid16 :: (Ord a, Num a) => [a] -> Bool
valid16 [] = True
valid16 (x:xs) = (x <= 2 ^ 16 - 1) && (x >= 0) && (valid16 xs)

valid8 :: (Ord a, Num a) => [a] -> Bool
valid8 [] = True
valid8 (x:xs) = (x <= 2 ^ 8 - 1) && (x >= 0) && (valid8 xs)

prop_Crunch64 xs = let ws = map fromIntegral (xs :: [Int]) in
  valid64 ws ==> (decrunch64 $ crunch64 ws) == ws

prop_Crunch32 xs = let ws = map fromIntegral (xs :: [Int]) in
  valid32 ws ==> (decrunch32 $ crunch32 ws) == ws

prop_Crunch16 xs = let ws = map fromIntegral (xs :: [Int]) in
  valid16 ws ==> (decrunch16 $ crunch16 ws) == ws

prop_Crunch8 xs = let ws = map fromIntegral (xs :: [Int]) in
  valid8 ws ==> (decrunch8 $ crunch8 ws) == ws

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("Crunch tests",
                [ run prop_Crunch64
                , run prop_Crunch32
                , run prop_Crunch16
                , run prop_Crunch8
                ])

allTests :: Test  
allTests = TestLabel "Crunch tests" $ 
  TestList
  [
  ]
