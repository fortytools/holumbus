-- ----------------------------------------------------------------------------

{- |
  Module     : StrMapTest
  Copyright  : Copyright (C) 2007 Timo B. Hübel
  License    : MIT

  Maintainer : Timo B. Hübel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : $Id$

  The some unit tests for the Spoogle patricia trie.

-}

-- ----------------------------------------------------------------------------

module StrMapTest (allTests) where

import qualified Spoogle.Data.StrMap as SM

import Test.HUnit

emptyTests :: Test
emptyTests  = TestList 
  [ TestCase (assertEqual "Empty map should have no key" 0 
  (length (SM.elems SM.empty)))
  
  , TestCase (assertEqual "Empty map should have no elements" 0 
  (SM.size SM.empty))
  ]

insertTests :: Test
insertTests = TestList 
  [ TestCase (assertEqual "Inserting into empty map" [("a",1)] 
  (SM.toList (SM.insert "a" 1 SM.empty)))

  , TestCase (assertEqual "Inserting should split correctly" [("ac",1),("a",2)] 
  (SM.toList (SM.insert "a" 2 (SM.insert "ac" 1 SM.empty))))

  , TestCase (assertEqual "Insert should just concatenate" [("ac",2),("a",1)]
  (SM.toList (SM.insert "ac" 2 (SM.insert "a" 1 SM.empty))))

  , TestCase (assertEqual "Insert should add a new node" [("b",2),("a",1)]
  (SM.toList (SM.insert "b" 2 (SM.insert "a" 1 SM.empty))))

  , TestCase (assertEqual "Complex insert" [("ad",4),("acd",3),("abcd",1),("bcd",2)]
  (SM.toList (SM.insert "ad" 4 (SM.insert "acd" 3 (SM.insert "bcd" 2 (SM.insert "abcd" 1 SM.empty))))))
  ]
  
findTests :: Test  
findTests = TestList
  [ TestCase (assertEqual "The only element should be found" (Just 1)
  (SM.lookup "a" (SM.insert "a" 1 SM.empty)))
  
  , TestCase (assertEqual "Finding the only element" (Just 6)
  (SM.lookup "ace" (SM.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by prefix" [6, 5, 2]
  (SM.prefixFind "ac" (SM.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))
  ]

allTests :: Test  
allTests = TestLabel "StrMap tests" $ 
  TestList
  [ TestLabel "Empty tests" emptyTests
  , TestLabel "Insert tests" insertTests
  , TestLabel "Find tests" findTests
  ]