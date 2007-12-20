-- ----------------------------------------------------------------------------

{- |
  Module     : BiMapTest
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Holumbus bijective map.

-}

-- ----------------------------------------------------------------------------

module BiMapTest (allTests) where

import qualified Holumbus.Data.BiMap as BM

import qualified Data.Map as M

import Test.HUnit

emptyTests :: Test
emptyTests  = TestList 
  [ TestCase (assertEqual "Empty BiMap should have no elements" 0 (BM.size BM.empty))
  
  , TestCase $ do
  assertEqual "Querying for anything should return nothing" (Nothing :: Maybe Int) (BM.lookupA 1 BM.empty)
  assertEqual "Querying for anything should return nothing" (Nothing :: Maybe Int) (BM.lookupB 1 BM.empty)
  ]

insertTests :: Test
insertTests = TestList
  [ TestCase $ do
    let bm = BM.insert 1 2 BM.empty
    assertEqual "Should give the value for 1" (Just 2) (BM.lookupA 1 bm)
    assertEqual "Should give the value for 2" (Just 1) (BM.lookupB 2 bm)
    assertEqual "Size should be 1" 1 (BM.size bm)
    
  , TestCase (assertEqual "Insert into empty map should be the same as a singleton"
  (BM.singleton 1 2)
  (BM.insert 1 2 BM.empty))
  ]

conversionTests :: Test
conversionTests = TestList
  [ TestCase (assertEqual "Creating BiMap from a list" (BM.singleton 1 2)
  (BM.fromList [(1, 2)]))
  
  , TestCase (assertEqual "Creating BiMap from a map" (BM.singleton 1 2)
  (BM.fromMap (M.singleton 1 2)))
  
  , TestCase $ do
  let l = [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10)]
  assertEqual "Length of list and BiMap has to be identical" (length l) (BM.size $ BM.fromList l)
  ]

allTests :: Test  
allTests = TestLabel "BiMap tests" $ 
  TestList
  [ TestLabel "Empty tests" emptyTests
  , TestLabel "Insert tests" insertTests
  , TestLabel "Conversion tests" conversionTests
  ]