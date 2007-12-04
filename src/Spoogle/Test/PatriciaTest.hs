-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Test.PatriciaTest
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   The some unit tests for the Spoogle patricia trie.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Test.PatriciaTest (allTests) where

import qualified Spoogle.Data.Patricia as P

import Test.HUnit

testEmpty1 = TestCase (assertEqual "Empty node should have no key" "" 
  (P.key P.empty))

testEmpty2 = TestCase (assertEqual "Empty node should have no elements" 0 
  (P.size P.empty))

testInsert1 = TestCase (assertEqual "Inserting into empty node" [("a",1)] 
  (P.toList (P.insert "a" 1 P.empty)))

testInsert2 = TestCase (assertEqual "Inserting should split nodes" [("ac",1),("a",2)] 
  (P.toList (P.insert "a" 2 (P.insert "ac" 1 P.empty))))

testInsert3 = TestCase (assertEqual "Insert should just concatenate" [("ac",2),("a",1)]
  (P.toList (P.insert "ac" 2 (P.insert "a" 1 P.empty))))

testInsert4 = TestCase (assertEqual "Insert should add a new node" [("b",2),("a",1)]
  (P.toList (P.insert "b" 2 (P.insert "a" 1 P.empty))))

testInsert5 = TestCase (assertEqual "Complex insert" [("ad",4),("acd",3),("abcd",1),("bcd",2)]
  (P.toList (P.insert "ad" 4 (P.insert "acd" 3 (P.insert "bcd" 2 (P.insert "abcd" 1 P.empty))))))

emptyTests  = TestList 
  [ TestLabel "Empty key" testEmpty1
  , TestLabel "Empty size" testEmpty2
  ]

insertTests = TestList 
  [ TestLabel "Insert on empty node" testInsert1
  , TestLabel "Splitting nodes" testInsert2
  , TestLabel "Concatenating nodes" testInsert3
  , TestLabel "Adding nodes" testInsert4
  , TestLabel "Complex insert" testInsert5
  ]

allTests = TestLabel "Patricia tests" $ 
  TestList
  [ TestLabel "Empty tests" emptyTests
  , TestLabel "Insert tests" insertTests
  ]