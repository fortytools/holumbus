-- ----------------------------------------------------------------------------

{- |
  Module     : FuzzyTest
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Holumbus fuzzy set generator.

-}

-- ----------------------------------------------------------------------------

module FuzzyTest (allTests) where

import Holumbus.Query.Fuzzy

import Test.HUnit

replaceTests :: Test
replaceTests = TestList 
  [ TestCase (assertEqual "Simple replace"
  [("fbc", 1.0)]
  (toList $ fuzz (FuzzyConfig True False 1.0 [(("a", "f"), 1.0)]) "abc"))

  , TestCase (assertEqual "Replace in both directions"
  [("aac", 2/3), ("bbc", 1.0)]
  (toList $ fuzz (FuzzyConfig True False 1.0 [(("a", "b"), 1.0)]) "abc"))

  , TestCase (assertEqual "Calculating scores depending on position"
  [("abcda", 0.2), ("ebcde", 1.0)]
  (toList $ fuzz (FuzzyConfig True False 1.0 [(("a", "e"), 1.0)]) "abcde"))

  , TestCase (assertEqual "Calculating scores depending on position and weight"
  [("abcda", 0.1), ("ebcde", 0.5), ("abbde", 0.6), ("ebcda", 0.7), ("abbda", 0.8), ("accde", 0.8), ("accda", 1.0)]
  (toList $ fuzz (FuzzyConfig True False 1.0 [(("a", "e"), 0.4), (("b", "c"), 0.8)]) "abcde"))

  , TestCase (assertEqual "Balancing weights correctly"
  [("abcda", 0.1), ("ebcde", 0.5), ("abbde", 0.6), ("ebcda", 0.7), ("abbda", 0.8), ("accde", 0.8), ("accda", 1.0)]
  (toList $ fuzz (FuzzyConfig True False 1.0 [(("a", "e"), 1.0), (("b", "c"), 2.0)]) "abcde"))
  ]

swapTests :: Test
swapTests = TestList 
  [ TestCase (assertEqual "Correctly swap all characters"
  [("acb", 2/3), ("bac", 1.0)]
  (toList $ fuzz (FuzzyConfig False True 1.0 []) "abc"))

  , TestCase (assertEqual "Correctly calculating scores"
  [("abced", 0.4), ("abdce", 0.6), ("acbde", 0.8), ("bacde", 1.0)]
  (toList $ fuzz (FuzzyConfig False True 1.0 []) "abcde"))

  , TestCase (assertEqual "Swap alot"
  [("abcdefghji", 0.2), ("abcdefgihj", 0.3), ("abcdefhgij", 0.4), ("abcdegfhij", 0.5)
  , ("abcdfeghij", 0.6), ("abcedfghij", 0.7)]
  (toList $ fuzz (FuzzyConfig False True 0.7 []) "abcdefghij"))
  ]

allTests :: Test
allTests = TestLabel "Fuzzy tests" $
  TestList 
  [ TestLabel "Replace tests" replaceTests
  , TestLabel "Swap tests" swapTests 
  ]