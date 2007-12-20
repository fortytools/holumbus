-- ----------------------------------------------------------------------------

{- |
  Module     : StrMapTest
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Holumbus patricia trie.

-}

-- ----------------------------------------------------------------------------

module StrMapTest (allTests) where

import Data.List

import qualified Holumbus.Data.StrMap as SM

import Test.HUnit

emptyTests :: Test
emptyTests  = TestList 
  [ TestCase (assertEqual "Empty map should have no key" ([] :: [Int])
  (SM.elems SM.empty))
  
  , TestCase (assertEqual "Empty map should have no elements" 0 
  (SM.size SM.empty))
  
  , TestCase (assertEqual "Querying empty map for anything should give nothing" (Nothing :: Maybe Int)
  (SM.lookup "test" SM.empty))
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
  
  , TestCase (assertEqual "1. case: Existing key"
  (SM.singleton "abc" 2)
  (SM.insert "abc" 2 (SM.singleton "abc" 1)))

--  , TestCase (assertEqual "2. case: Insert into list of successors"
--  (Seq "" [End "ab" 1 [End "c" 2 []]]) -- TODO: Make Seq and End avaliable for testing purposes.
--  (SM.insert "abc" 2 (SM.singleton "ab" 1)))
  
  , TestCase (assertEqual "Inserting in different order shold yield the same result"
  (SM.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])
  (SM.fromList [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("aCf", 5), ("Ab", 4), ("Ac", 3), ("aB", 2), ("a", 1)]))

  , TestCase (assertEqual "Combining function should preserve old value"
  [("abc", 3), ("a", 1)]
  (SM.toList (SM.insertWithKey (\k nv ov -> if k == "abc" then ov else nv) "abc" 4 (SM.fromList [("a", 1), ("abc", 3)]))))

  , TestCase (assertEqual "Combining function should give new value"
  [("abd", 4), ("a", 1)]
  (SM.toList (SM.insertWithKey (\k nv ov -> if k == "abc" then ov else nv) "abd" 4 (SM.fromList [("a", 1), ("abd", 3)]))))
  ]

findTests :: Test  
findTests = TestList
  [ TestCase (assertEqual "The only element should be found" (Just 1)
  (SM.lookup "a" (SM.insert "a" 1 SM.empty)))
  
  , TestCase (assertEqual "Finding the only element" (Just 6)
  (SM.lookup "ace" (SM.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by prefix" [2, 5, 6]
  $ sort (SM.prefixFind "ac" (SM.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by prefix" [5, 6, 7]
  $ sort (SM.prefixFind "ac" (SM.fromList [("a", 1), ("b", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6), ("ac", 7)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [2, 5]
  $ sort (SM.lookupNoCase "aC" (SM.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("Ac", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [2, 5, 6]
  $ sort (SM.lookupNoCase "ACE" (SM.fromList [("Acg", 1), ("aCe", 2), ("acd", 3), ("AcF", 4), ("AcE", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [5, 6]
  $ sort (SM.lookupNoCase "ACE" (SM.fromList [("a", 1), ("A", 2), ("aC", 3), ("Ac", 4), ("AcE", 5), ("aCe", 6), ("aCef", 7), ("AcEf", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive" [2, 3, 4, 5, 6, 7, 8]
  $ sort (SM.prefixFindNoCase "ac" (SM.fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive" [3, 5, 6, 7, 8]
  $ sort (SM.prefixFindNoCase "ac" (SM.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive with key" [("aCf", 5), ("aCE", 4), ("aC", 2), ("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3)]
  (SM.prefixFindNoCaseWithKey "ac" (SM.fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive with key" [("aCf", 5), ("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3)]
  (SM.prefixFindNoCaseWithKey "ac" (SM.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
  ]

allTests :: Test  
allTests = TestLabel "StrMap tests" $ 
  TestList
  [ TestLabel "Empty tests" emptyTests
  , TestLabel "Insert tests" insertTests
  , TestLabel "Find tests" findTests
  ]