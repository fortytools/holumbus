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
  [("acb", 2/3), ("bac", 1.0), ("fbc", 1.0)]
  (toList $ fuzzWith [(("a", "f"), 1.0)] "abc"))

  , TestCase (assertEqual "Replace in both directions"
  [("aac", 2/3), ("acb", 2/3), ("bac", 1.0), ("bbc", 1.0)]
  (toList $ fuzzWith [(("a", "b"), 1.0)] "abc"))

  , TestCase (assertEqual "Calculating scores depending on position"
  [("abcda", 0.2), ("abced", 0.4), ("abdce", 0.6), ("acbde", 0.8), ("bacde", 1.0), ("ebcde", 1.0)]
  (toList $ fuzzWith [(("a", "e"), 1.0)] "abcde"))

  , TestCase (assertEqual "Calculating scores depending on position and weight"
  [("abcda", 0.1), ("abced", 0.4), ("ebcde", 0.5), ("abbde", 0.6), ("abdce", 0.6), ("acbde", 0.8), ("accde", 0.8), ("bacde", 1.0)]
  (toList $ fuzzWith [(("a", "e"), 0.4), (("b", "c"), 0.8)] "abcde"))

  , TestCase (assertEqual "Balancing weights correctly"
  [("abcda", 0.1), ("abced", 0.4), ("ebcde", 0.5), ("abbde", 0.6), ("abdce", 0.6), ("acbde", 0.8), ("accde", 0.8), ("bacde", 1.0)]
  (toList $ fuzzWith [(("a", "e"), 1.0), (("b", "c"), 2.0)] "abcde"))
  ]

swapTests :: Test
swapTests = TestList 
  [ TestCase (assertEqual "Correctly swap all characters"
  [("acb", 2/3), ("bac", 1.0)]
  (toList $ fuzzWith [] "abc"))

  , TestCase (assertEqual "Correctly calculating scores"
  [("abced", 0.4), ("abdce", 0.6), ("acbde", 0.8), ("bacde", 1.0)]
  (toList $ fuzzWith [] "abcde"))

  , TestCase (assertEqual "Swap alot"
  [("abcdefghji", 0.2), ("abcdefgihj", 0.3), ("abcdefhgij", 0.4), ("abcdegfhij", 0.5), ("abcdfeghij", 0.6), 
  ("abcedfghij", 0.7), ("abdcefghij", 0.8), ("acbdefghij", 0.9), ("bacdefghij", 1.0)]
  (toList $ fuzzWith [] "abcdefghij"))
  ]

fuzzMoreTests :: Test
fuzzMoreTests = TestList
  [ TestCase (assertEqual "More swapping"
  [("abc", 4/3), ("bca", 1.0 + 2/3), ("cab", 1.0 + 2/3)]
  (toList $ fuzzMoreWith [] $ fuzzWith [] "abc"))

  , TestCase (assertEqual "More replacing"
  [("abc", 4/3), ("aeb", 4/3), ("bae", 1.0 + 1/3), ("fbe", 1.0 + 1/3), ("bca", 1.0 + 2/3), 
   ("cab", 1.0 + 2/3), ("fcb", 1.0 + 2/3), ("bfc", 2.0)]
  (toList $ fuzzMoreWith [(("c", "e"), 1.0)] $ fuzzWith [(("a", "f"), 1.0)] "abc"))
  ]

allTests :: Test
allTests = TestLabel "Fuzzy tests" $
  TestList 
  [ TestLabel "Replace tests" replaceTests
  , TestLabel "Swap tests" swapTests 
  , TestLabel "Fuzz more tests" fuzzMoreTests
  ]