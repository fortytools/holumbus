-- ----------------------------------------------------------------------------

{- |
  Module     : Pickle
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Holumbus bijective map.

-}

-- ----------------------------------------------------------------------------

module PickleTest (allTests) where

import Holumbus.Index.Inverted
import Holumbus.Index.Documents

import Data.Maybe

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Holumbus.Data.StrMap as SM

import Text.XML.HXT.Arrow

import Test.HUnit

testIndex1, testIndex2 :: InvIndex
testIndex1 = empty

testIndex2 = InvIndex docs parts
  where
  docs = Documents
         (IM.fromList [(1, ("doc1","uri1")), (2, ("doc2","uri2")), (3, ("doc3","uri3"))])
         (M.fromList [("uri1", 1), ("uri2", 2), ("uri3", 3)])
         3
  parts = M.fromList [("context1", context1), ("context2", context2), ("context3", context3)]
  context1 = SM.fromList [ ("word1", IM.fromList [(1, IS.fromList [12,23,43]), (2, IS.fromList [10,2,4])])
                         , ("word2", IM.fromList [(3, IS.fromList [15,33,45]), (2, IS.fromList [1,32,94])])
                         , ("word3", IM.fromList [(3, IS.fromList [18,43,15]), (1, IS.fromList [18,82,64])])
                         ]
  context2 = SM.fromList [ ("word2", IM.fromList [(1, IS.fromList [29,63,73]), (2, IS.fromList [51,28,45])])
                         , ("word4", IM.fromList [(2, IS.fromList [5,65,47]), (3, IS.fromList [13,32,14])])
                         , ("word5", IM.fromList [(3, IS.fromList [8,48,55])])
                         ]
  context3 = SM.fromList [ ("word2", IM.fromList [(1, IS.fromList [24,60,77]), (3, IS.fromList [21,26,25])])
                         , ("word5", IM.fromList [(3, IS.fromList [86,78,35])])
                         ]

-- Stolen from the HXT pickle tests. Thanks :)
pickleUnpickleTests :: Test
pickleUnpickleTests = TestLabel "Pickle/unpickle tests with inverted indexes" $
                      TestList $ map makeTests [testIndex1, testIndex2]
                      where
                      makeTests i = TestList $
                        [ TestCase $
                          assertEqual "pickleDoc/unpickleDoc without XML serialisation: " [i] res1
  
                        , TestCase $
                          assertEqual "pickleDoc/unpickleDoc with xshow/xread: " [i] res2
  
                        , TestCase $
                          do
                          res <- res4
                          assertEqual "pickle/unpickle with readFromString: " [i] res
  
                        , TestCase $
                          res5 >>= 
                          assertEqual "pickle/unpickle with writeDocument/readDocument: " [i]
  
                        , TestCase $
                          res6 >>= 
                          assertEqual "pickle/unpickle with xpickleDocument/xunpickleDocument: " [i]
{-
FIXME TH 15.01.2008: See below
                        , TestCase $
                          res7 >>= 
                          assertEqual "pickle/unpickle with DTD validation xpickleDocument/xunpickleDocument: " [i]
-}
                        ]
                        where
                        res1 :: [InvIndex]
                        res1 = maybeToList . unpickleDoc xpInvIndex . pickleDoc xpInvIndex $ i
                      
                        res2 :: [InvIndex]
                        res2 = runLA (xshow (arr (pickleDoc xpInvIndex) 
                                 >>> getChildren)
                                 >>> root [] [xread]
                                 >>> arrL (maybeToList . unpickleDoc xpInvIndex)) i
                      
                        res4 :: IO [InvIndex]
                        res4 = runX (constA i
                                >>> arr (pickleDoc xpInvIndex)                   -- InvIndex => XmlTree
                                >>> writeDocumentToString []                     -- XmlTree => String
                                >>> readFromString [(a_validate, v_0)]           -- String => XmlTree
                                >>> arrL (maybeToList . unpickleDoc xpInvIndex)) -- XmlTree => InvIndex
                      
                        res5 :: IO [InvIndex] 
                        res5 = runX (constA i                                    -- Take the InvIndex value
                                >>> arr (pickleDoc xpInvIndex)                   -- InvIndex => XmlTree
                                >>> writeDocument [(a_indent, v_1)] "pickle.xml" -- XmlTree => formated external XML document
                                >>> readDocument  [(a_remove_whitespace, v_1), (a_validate, v_0)] "pickle.xml" -- Formated external XML document => XmlTree 
                                >>> arrL (maybeToList . unpickleDoc xpInvIndex)) -- XmlTree => InvIndex
                      
                        res6 :: IO [InvIndex]                                    -- Same as above the convinient way
                        res6 = runX (constA i
                                >>> xpickleDocument xpInvIndex [(a_indent, v_1)] "pickle.xml"
                                >>> xunpickleDocument xpInvIndex [(a_remove_whitespace, v_1), (a_validate, v_0)] "pickle.xml")

{-
FIXME TH 15.01.2008: Adding a DTD automatically does not work yet, because we use
                     the doc element twice with different meanings: Once as part of the
                     document table and as part of an index, too  
                        res7 :: IO [InvIndex]                                    -- Same as above with validation
                        res7 = runX (constA i
                                >>> xpickleDocument xpInvIndex [(a_indent, v_1), (a_addDTD, v_1)] "pickle.xml"
                                >>> xunpickleDocument xpInvIndex [(a_remove_whitespace, v_1), (a_validate, v_1)] "pickle.xml")
--}

allTests :: Test  
allTests = pickleUnpickleTests
