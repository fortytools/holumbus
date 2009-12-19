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

{-# OPTIONS -fno-warn-type-defaults  -fno-warn-orphans -fno-warn-missing-signatures #-}

module PrefixTreeTest (allTests, allProperties) where

import Data.List
import Data.Char
import Data.Binary
import Data.Function

import qualified Data.Map as M

import qualified Holumbus.Data.PrefixTree 	as PT
import qualified PrefixTreeTest2		as PT2

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

-- ----------------------------------------
-- HUnit tests


allTests :: Test  
allTests = TestLabel "PrefixTree tests" $ 
  TestList
  [ TestLabel "Empty tests"  emptyTests
  , TestLabel "Insert tests" insertTests
  , TestLabel "Find tests"   findTests
  , PT2.allTests
  ]

-- ----------------------------------------

emptyTests :: Test
emptyTests  = TestList 
  [ TestCase (assertEqual "Empty map should have no key" ([] :: [Int])
  (PT.elems PT.empty))
  
  , TestCase (assertEqual "Empty map should have no elements" 0 
  (PT.size PT.empty))
  
  , TestCase (assertEqual "Querying empty map for anything should give nothing" (Nothing :: Maybe Int)
  (PT.lookup "test" PT.empty))
  ]

insertTests :: Test
insertTests = TestList 
  [ TestCase (assertEqual "Inserting into empty map" [("a",1)] 
    (PT.toList (PT.insert "a" 1 PT.empty)))

  , TestCase (assertEqual "Inserting should split correctly" (sort [("ac",1),("a",2)])
    (PT.toList (PT.insert "a" 2 (PT.insert "ac" 1 PT.empty))))

  , TestCase (assertEqual "Insert should just concatenate" (sort [("ac",2),("a",1)])
    (PT.toList (PT.insert "ac" 2 (PT.insert "a" 1 PT.empty))))

  , TestCase (assertEqual "Insert should add a new node" (sort [("b",2),("a",1)])
    (PT.toList (PT.insert "b" 2 (PT.insert "a" 1 PT.empty))))

  , TestCase (assertEqual "Complex insert" (sort [("ad",4),("acd",3),("abcd",1),("bcd",2)])
    (PT.toList (PT.insert "ad" 4 (PT.insert "acd" 3 (PT.insert "bcd" 2 (PT.insert "abcd" 1 PT.empty))))))
  
  , TestCase (assertEqual "Inserting in different order shold yield the same result"
    (PT.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])
    (PT.fromList [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("aCf", 5), ("Ab", 4), ("Ac", 3), ("aB", 2), ("a", 1)]))

  , TestCase (assertEqual "Combining function should preserve old value"
    (sort [("abc", 3), ("a", 1)])
    (PT.toList (PT.insertWithKey (\k nv ov -> if k == "abc" then ov else nv) "abc" 4 (PT.fromList [("a", 1), ("abc", 3)]))))

  , TestCase (assertEqual "Combining function should give new value"
    (sort [("abd", 4), ("a", 1)])
    (PT.toList (PT.insertWithKey (\k nv ov -> if k == "abc" then ov else nv) "abd" 4 (PT.fromList [("a", 1), ("abd", 3)]))))
  ]

findTests :: Test  
findTests = TestList
  [ TestCase (assertEqual "The only element should be found" (Just 1)
    (PT.lookup "a" (PT.insert "a" 1 PT.empty)))
  
  , TestCase (assertEqual "Finding the only element" (Just 6)
    (PT.lookup "ace" (PT.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by prefix" [2, 5, 6]
    $ sort (PT.prefixFind "ac" (PT.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by prefix" [5, 6, 7]
    $ sort (PT.prefixFind "ac" (PT.fromList [("a", 1), ("b", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6), ("ac", 7)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [("Ac", 5), ("ac", 2)]
    $ sort (PT.lookupNoCase "aC" (PT.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("Ac", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [("AcE", 5), ("aCe", 2), ("ace", 6)]
    $ sort (PT.lookupNoCase "ACE" (PT.fromList [("Acg", 1), ("aCe", 2), ("acd", 3), ("AcF", 4), ("AcE", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [("AcE", 5), ("aCe", 6)]
    $ sort (PT.lookupNoCase "ACE" (PT.fromList [("a", 1), ("A", 2), ("aC", 3), ("Ac", 4), ("AcE", 5), ("aCe", 6), ("aCef", 7), ("AcEf", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive" [2, 3, 4, 5, 6, 7, 8]
    $ sort (PT.prefixFindNoCase "ac" (PT.fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive" [3, 5, 6, 7, 8]
    $ sort (PT.prefixFindNoCase "ac" (PT.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive with key"
    (sort [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3), ("aCf", 5), ("aCE", 4), ("aC", 2)])
    (PT.prefixFindNoCaseWithKey "ac" (PT.fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive with key"
    (sort [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3), ("aCf", 5)])
    (PT.prefixFindNoCaseWithKey "ac" (PT.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
  ]

-- ----------------------------------------

instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)

valid :: [(String, Int)] -> Bool
valid [] = True
valid ((k, _):xs) = k /= "" && valid xs

clean :: [(String, a)] -> [(String, a)]
clean = (nubBy (\(k1, _) (k2, _) -> k1 == k2)) . normal

normal :: [(String, a)] -> [(String, a)]
normal = sortBy (compare `on` fst)

prop_FromToList xs = valid xs
  ==> normal (PT.toList (PT.fromList $ clean xs)) == clean xs

prop_FromToMap xs = valid xs 
  ==> PT.toMap (PT.fromMap (M.fromList xs)) == M.fromList xs

prop_EqMapList xs = valid xs 
  ==> (PT.fromList $ clean xs) == (PT.fromMap $ M.fromList xs)

prop_InsertLookup xs k v = (valid xs) && k /= "" 
  ==> PT.lookup k (PT.insert k v (PT.fromList xs)) == Just v

prop_Equal xs = valid xs
  ==> PT.fromList (reverse $ clean xs) == PT.fromList (clean xs)

prop_Binary xs = let sm = (PT.fromList xs) in 
  valid xs 
  ==> ((decode . encode) sm) == sm

prop_Map xs = valid xs 
  ==> (PT.map ((*) 2) (PT.fromList $ clean xs)) == PT.fromList (map (\(k, v) -> (k, v * 2)) (clean xs))

prop_Fold xs = valid xs 
  ==> (PT.fold (+) 0 (PT.fromList $ clean xs)) == (foldr (\(_, v) r -> v + r) 0 (clean xs)) 

prop_DeleteEmpty k = k /= "" 
  ==> (PT.delete k (PT.insert k 1 PT.empty)) == (PT.empty)

prop_DeleteInsert k xs = let sm = (PT.fromList xs) in 
  valid xs && k /= "" && (PT.member k sm == False) 
  ==> PT.delete k (PT.insert k 1 sm) == sm

prop_DeleteLookup k xs = let sm = (PT.fromList xs) in
  valid xs && k /= ""
  ==> PT.lookup k (PT.delete k sm) == Nothing
  
prop_Delete k xs = let sm = (PT.fromList xs) in
  valid xs && k /= "" && (PT.member k sm == False)
  ==> PT.delete k sm == sm

prop_LookupNoCase xs k v = (valid xs) && k /= ""
  ==> (map toLower k, v) `elem` (PT.lookupNoCase k (PT.insert (map toLower k) v (PT.fromList xs)))

prop_Null k = k /= ""
  ==> PT.null (PT.delete k (PT.insert k 1 PT.empty)) == True

prop_IndexOperator xs k = let sm = (PT.fromList xs) in
  valid xs && k /= "" 
  ==> ((PT.insert k 1 sm) PT.! k) == 1

prop_InsertWith xs k = let sm = (PT.fromList xs) in
  valid xs && k /= ""
  ==> PT.insertWith (flip const) k 2 (PT.insert k 1 sm) == PT.insert k 1 sm

prop_UpdateDelete xs k = let sm = (PT.fromList xs) in
  valid xs && k /= ""
  ==> PT.update (const Nothing) k sm == PT.delete k sm

prop_UpdateInsert xs k = let sm = (PT.fromList xs) in
  valid xs && k /= ""
  ==> PT.update (const (Just 2)) k sm == (if PT.member k sm then PT.insert k 2 sm else sm)

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("StrMap tests",
                [ run prop_FromToList
                , run prop_FromToMap
                , run prop_EqMapList
                , run prop_InsertLookup
                , run prop_Equal
                , run prop_Binary
                , run prop_Map
                , run prop_Fold
                , run prop_Delete
                , run prop_DeleteEmpty
                , run prop_DeleteInsert
                , run prop_DeleteLookup
                , run prop_LookupNoCase
                , run prop_Null
                , run prop_IndexOperator
                , run prop_InsertWith
                , run prop_UpdateDelete
                , run prop_UpdateInsert
                ])

-- ----------------------------------------
