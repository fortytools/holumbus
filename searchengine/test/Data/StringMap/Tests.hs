-- ----------------------------------------------------------------------------

{- |
  Module     : StringMapTest
  Copyright  : Copyright (C) 2009 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Holumbus patricia trie.

-}

-- ----------------------------------------------------------------------------

-- {-# OPTIONS -fno-warn-type-defaults  -fno-warn-orphans -fno-warn-missing-signatures #-}

module Data.StringMap.Tests -- (allTests {-, allProperties-})
where

import           Prelude		hiding ( lookup, map, null )

import           Data.List              ( sort, sortBy, nubBy )
import           Data.Char
import           Data.Binary
import           Data.Function
import qualified Data.Map 		as M
import           Data.StringMap.Class

import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Batch

-- ----------------------------------------------------------------------------

allTests 	:: ( StringMap 	    m
                   , StringMapFuzzy m
                   , Eq 	   (m Int)
                   , Show 	   (m Int)
                   ) =>
                   m Int -> ([(String, Int)] -> m Int)-> Test
allTests empty fromList
    = TestLabel "StrMap tests" $ 
      TestList
      [ TestLabel "Empty tests"	 emptyTests
      , TestLabel "Insert tests" insertTests
      , TestLabel "Find tests"	 findTests
      ]
    where
    emptyTests :: Test
    emptyTests
        = TestList 
	  [ TestCase (assertEqual "Empty map should have no key"
		      ([] :: [Int])
		      (elems empty))
	  , TestCase (assertEqual "Empty map should have no elements"
		      0 
		      (size empty))
	  , TestCase (assertEqual "Querying empty map for anything should give nothing"
		      (Nothing :: Maybe Int)
		      (lookup "test" empty))
	  ]
    insertTests :: Test
    insertTests
        = TestList 
          [ TestCase (assertEqual "Inserting into empty map"
                      [("a",1)] 
                      (toList (insert "a" 1 empty)))
          , TestCase (assertEqual "Inserting should split correctly"
                      [("ac",1),("a",2)] 
                      (toList (insert "a" 2 (insert "ac" 1 empty))))
          , TestCase (assertEqual "Insert should just concatenate"
                      [("ac",2),("a",1)]
                      (toList (insert "ac" 2 (insert "a" 1 empty))))
          , TestCase (assertEqual "Insert should add a new node"
                      [("b",2),("a",1)]
                      (toList (insert "b" 2 (insert "a" 1 empty))))
          , TestCase (assertEqual "Complex insert"
                      [("ad",4),("acd",3),("abcd",1),("bcd",2)]
                      (toList (insert "ad" 4 (insert "acd" 3 (insert "bcd" 2 (insert "abcd" 1 empty))))))
          , TestCase (assertEqual "Inserting in different order shold yield the same result"
                      (fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])
                      (fromList [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("aCf", 5), ("Ab", 4), ("Ac", 3), ("aB", 2), ("a", 1)]))
          , TestCase (assertEqual "Combining function should preserve old value"
                      [("abc", 3), ("a", 1)]
                      (toList (insertWithKey (\k nv ov -> if k == "abc" then ov else nv) "abc" 4 (fromList [("a", 1), ("abc", 3)]))))
          , TestCase (assertEqual "Combining function should give new value"
                      [("abd", 4), ("a", 1)]
                      (toList (insertWithKey (\k nv ov -> if k == "abc" then ov else nv) "abd" 4 (fromList [("a", 1), ("abd", 3)]))))
          ]
    findTests :: Test
    findTests
        = TestList
          [ TestCase (assertEqual "The only element should be found"
                      (Just 1)
                      (lookup "a" (insert "a" 1 empty)))
          , TestCase (assertEqual "Finding the only element"
                      (Just 6)
                      (lookup "ace" (fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))
          , TestCase (assertEqual "Finding some elements by prefix"
                      [2, 5, 6]
                      $ sort (prefixFind "ac" (fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6)])))
          , TestCase (assertEqual "Finding some elements by prefix"
                      [5, 6, 7]
                      $ sort (prefixFind "ac" (fromList [("a", 1), ("b", 2), ("ab", 3), ("ad", 4), ("acd", 5), ("ace", 6), ("ac", 7)])))
          , TestCase (assertEqual "Finding some elements by case insensitive match"
                      [("Ac", 5), ("ac", 2)]
                      $ sort (lookupNoCase "aC" (fromList [("a", 1), ("ac", 2), ("ab", 3), ("ad", 4), ("Ac", 5), ("ace", 6)])))
          , TestCase (assertEqual "Finding some elements by case insensitive match"
                      [("AcE", 5), ("aCe", 2), ("ace", 6)]
                      $ sort (lookupNoCase "ACE" (fromList [("Acg", 1), ("aCe", 2), ("acd", 3), ("AcF", 4), ("AcE", 5), ("ace", 6)])))
          , TestCase (assertEqual "Finding some elements by case insensitive match"
                      [("AcE", 5), ("aCe", 6)]
                      $ sort (lookupNoCase "ACE" (fromList [("a", 1), ("A", 2), ("aC", 3), ("Ac", 4), ("AcE", 5), ("aCe", 6), ("aCef", 7), ("AcEf", 8)])))
          , TestCase (assertEqual "Prefix find case insensitive"
                      [2, 3, 4, 5, 6, 7, 8]
                      $ sort (prefixFindNoCase "ac" (fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
          , TestCase (assertEqual "Prefix find case insensitive"
                      [3, 5, 6, 7, 8]
                      $ sort (prefixFindNoCase "ac" (fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
          , TestCase (assertEqual "Prefix find case insensitive with key"
                      [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3), ("aCf", 5), ("aCE", 4), ("aC", 2)]
                      (prefixFindNoCaseWithKey "ac" (fromList [("a", 1), ("aC", 2), ("Ac", 3), ("aCE", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
          , TestCase (assertEqual "Prefix find case insensitive with key"
                      [("Aceg", 8), ("Acef", 7), ("Ace", 6), ("Ac", 3), ("aCf", 5)]
                      (prefixFindNoCaseWithKey "ac" (fromList [("a", 1), ("aB", 2), ("Ac", 3), ("Ab", 4), ("aCf", 5), ("Ace", 6), ("Acef", 7), ("Aceg", 8)])))
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

{-
-}

allProperties 	:: ( StringMap 	    m
                   , StringMapFuzzy m
                   , Eq 	   (m Int)
                   , Show 	   (m Int)
                   , Binary        (m Int)
                   ) =>
                   m Int ->
                   ([(String, Int)] -> m Int) ->
                   (M.Map String Int -> m Int) ->
                   (String, [TestOptions -> IO TestResult])

allProperties empty fromList fromMap
    = ("StrMap tests",
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
    where
    prop_FromToList xs
        = valid xs
          ==> normal (toList (fromList $ clean xs)) == clean xs
    prop_FromToMap xs
        = valid xs 
          ==> toMap (fromMap (M.fromList xs)) == M.fromList xs
    prop_EqMapList xs
        = valid xs 
          ==> (fromList $ clean xs) == (fromMap $ M.fromList xs)
    prop_InsertLookup xs k v
        = (valid xs) && k /= "" 
          ==> lookup k (insert k v (fromList xs)) == Just v
    prop_Equal xs
        = valid xs
          ==> fromList (reverse $ clean xs) == fromList (clean xs)
    prop_Binary xs
        = let sm = (fromList xs) in 
          valid xs 
          ==> ((decode . encode) sm) == sm
    prop_Map xs
        = valid xs 
          ==> (map ((*) 2) (fromList $ clean xs)) == fromList (fmap (\(k, v) -> (k, v * 2)) (clean xs))
    prop_Fold xs
        = valid xs 
          ==> (fold (+) 0 (fromList $ clean xs)) == (foldr (\(_, v) r -> v + r) 0 (clean xs)) 
    prop_DeleteEmpty k =
        k /= "" 
        ==> (delete k (insert k 1 empty)) == (empty)
    prop_DeleteInsert k xs
        = let sm = (fromList xs) in 
          valid xs && k /= "" && (member k sm == False) 
          ==> delete k (insert k 1 sm) == sm
    prop_DeleteLookup k xs
        = let sm = (fromList xs) in
          valid xs && k /= ""
          ==> lookup k (delete k sm) == Nothing
    prop_Delete k xs
        = let sm = (fromList xs) in
          valid xs && k /= "" && (member k sm == False)
          ==> delete k sm == sm
    prop_LookupNoCase xs k v
        = (valid xs) && k /= ""
          ==> (fmap toLower k, v) `elem` (lookupNoCase k (insert (fmap toLower k) v (fromList xs)))
    prop_Null k
        = k /= ""
          ==> null (delete k (insert k 1 empty)) == True
    prop_IndexOperator xs k
        = let sm = (fromList xs) in
          valid xs && k /= "" 
          ==> ((insert k 1 sm) ! k) == 1
    prop_InsertWith xs k
        = let sm = (fromList xs) in
          valid xs && k /= ""
          ==> insertWith (flip const) k 2 (insert k 1 sm) == insert k 1 sm
    prop_UpdateDelete xs k
        = let sm = (fromList xs) in
          valid xs && k /= ""
          ==> update (const Nothing) k sm == delete k sm
    prop_UpdateInsert xs k
        = let sm = (fromList xs) in
          valid xs && k /= ""
          ==> update (const (Just 2)) k sm == (if member k sm then insert k 2 sm else sm)

-- ------------------------------------------------------------
