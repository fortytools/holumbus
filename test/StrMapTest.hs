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

module StrMapTest (allTests, allProperties) where

import Data.List
import Data.Char
import Data.Binary
import Data.Function

import qualified Data.Map as M

import qualified Holumbus.Data.StrMap as SM

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

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

  , TestCase (assertEqual "Finding some elements by case insensitive match" [("Ac", 5), ("ac", 2)]
  $ sort (SM.lookupNoCase "aC" (SM.fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("Ac", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [("AcE", 5), ("aCe", 2), ("ace", 6)]
  $ sort (SM.lookupNoCase "ACE" (SM.fromList [("Acg", 1), ("aCe", 2), ("acd", 3), ("AcF", 4), ("AcE", 5), ("ace", 6)])))

  , TestCase (assertEqual "Finding some elements by case insensitive match" [("AcE", 5), ("aCe", 6)]
  $ sort (SM.lookupNoCase "ACE" (SM.fromList [("a", 1), ("A", 2), ("aC", 3), ("Ac", 4), ("AcE", 5), ("aCe", 6), ("aCef", 7), ("AcEf", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive" [2, 3, 4, 5, 6, 7, 8]
  $ sort (SM.prefixFindNoCase "ac" (SM.fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive" [3, 5, 6, 7, 8]
  $ sort (SM.prefixFindNoCase "ac" (SM.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive with key" [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3), ("aCf", 5), ("aCE", 4), ("aC", 2)]
  (SM.prefixFindNoCaseWithKey "ac" (SM.fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))

  , TestCase (assertEqual "Prefix find case insensitive with key" [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3), ("aCf", 5)]
  (SM.prefixFindNoCaseWithKey "ac" (SM.fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
  ]

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
  ==> normal (SM.toList (SM.fromList $ clean xs)) == clean xs

prop_FromToMap xs = valid xs 
  ==> SM.toMap (SM.fromMap (M.fromList xs)) == M.fromList xs

prop_EqMapList xs = valid xs 
  ==> (SM.fromList $ clean xs) == (SM.fromMap $ M.fromList xs)

prop_InsertLookup xs k v = (valid xs) && k /= "" 
  ==> SM.lookup k (SM.insert k v (SM.fromList xs)) == Just v

prop_Equal xs = valid xs
  ==> SM.fromList (reverse $ clean xs) == SM.fromList (clean xs)

prop_Binary xs = let sm = (SM.fromList xs) in 
  valid xs 
  ==> ((decode . encode) sm) == sm

prop_Map xs = valid xs 
  ==> (SM.map ((*) 2) (SM.fromList $ clean xs)) == SM.fromList (map (\(k, v) -> (k, v * 2)) (clean xs))

prop_Fold xs = valid xs 
  ==> (SM.fold (+) 0 (SM.fromList $ clean xs)) == (foldr (\(_, v) r -> v + r) 0 (clean xs)) 

prop_DeleteEmpty k = k /= "" 
  ==> (SM.delete k (SM.insert k 1 SM.empty)) == (SM.empty)

prop_DeleteInsert k xs = let sm = (SM.fromList xs) in 
  valid xs && k /= "" && (SM.member k sm == False) 
  ==> SM.delete k (SM.insert k 1 sm) == sm

prop_DeleteLookup k xs = let sm = (SM.fromList xs) in
  valid xs && k /= ""
  ==> SM.lookup k (SM.delete k sm) == Nothing
  
prop_Delete k xs = let sm = (SM.fromList xs) in
  valid xs && k /= "" && (SM.member k sm == False)
  ==> SM.delete k sm == sm

prop_LookupNoCase xs k v = (valid xs) && k /= ""
  ==> (map toLower k, v) `elem` (SM.lookupNoCase k (SM.insert (map toLower k) v (SM.fromList xs)))

prop_Null k = k /= ""
  ==> SM.null (SM.delete k (SM.insert k 1 SM.empty)) == True

prop_IndexOperator xs k = let sm = (SM.fromList xs) in
  valid xs && k /= "" 
  ==> ((SM.insert k 1 sm) SM.! k) == 1

prop_InsertWith xs k = let sm = (SM.fromList xs) in
  valid xs && k /= ""
  ==> SM.insertWith (flip const) k 2 (SM.insert k 1 sm) == SM.insert k 1 sm

prop_UpdateDelete xs k = let sm = (SM.fromList xs) in
  valid xs && k /= ""
  ==> SM.update (const Nothing) k sm == SM.delete k sm

prop_UpdateInsert xs k = let sm = (SM.fromList xs) in
  valid xs && k /= ""
  ==> SM.update (const (Just 2)) k sm == (if SM.member k sm then SM.insert k 2 sm else sm)

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

allTests :: Test  
allTests = TestLabel "StrMap tests" $ 
  TestList
  [ TestLabel "Empty tests" emptyTests
  , TestLabel "Insert tests" insertTests
  , TestLabel "Find tests" findTests
  ]
