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

emptyTests :: Test
emptyTests  = TestList 
  [ TestCase (assertEqual "Empty node should have no key" 0 
  (length (P.elems P.empty)))
  
  , TestCase (assertEqual "Empty node should have no elements" 0 
  (P.size P.empty))
  ]

insertTests :: Test
insertTests = TestList 
  [ TestCase (assertEqual "Inserting into empty node" [("a",1)] 
  (P.toList (P.insert "a" 1 P.empty)))

  , TestCase (assertEqual "Inserting should split nodes" [("ac",1),("a",2)] 
  (P.toList (P.insert "a" 2 (P.insert "ac" 1 P.empty))))

  , TestCase (assertEqual "Insert should just concatenate" [("ac",2),("a",1)]
  (P.toList (P.insert "ac" 2 (P.insert "a" 1 P.empty))))

  , TestCase (assertEqual "Insert should add a new node" [("b",2),("a",1)]
  (P.toList (P.insert "b" 2 (P.insert "a" 1 P.empty))))

  , TestCase (assertEqual "Complex insert" [("ad",4),("acd",3),("abcd",1),("bcd",2)]
  (P.toList (P.insert "ad" 4 (P.insert "acd" 3 (P.insert "bcd" 2 (P.insert "abcd" 1 P.empty))))))
  ]
  
findTests :: Test  
findTests = TestList
  [ TestCase (assertEqual "The only element should be found" (Just 1)
  (P.find "a" (P.insert "a" 1 P.empty)))
  
  , TestCase (assertEqual "Finding the only element" (Just 6)
  (P.find "ace" (P.toPat [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))
  ]

allTests :: Test  
allTests = TestLabel "Patricia tests" $ 
  TestList
  [ TestLabel "Empty tests" emptyTests
  , TestLabel "Insert tests" insertTests
  , TestLabel "Find tests" findTests
  ]