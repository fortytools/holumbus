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

import Holumbus.Index.Documents
import Holumbus.Index.Inverted.Memory
import Holumbus.Query.Result

import Data.Maybe

import Text.XML.HXT.Arrow

import Test.HUnit

import SampleData

testIndex1, testIndex2 :: Inverted
testIndex1 = emptyInverted
testIndex2 = sampleIndex1

testResult1, testResult2 :: Result Int
testResult1 = emptyResult
testResult2 = sampleResult1

testDocs1, testDocs2 :: Documents Int
testDocs1 = emptyDocuments
testDocs2 = sampleDocs1

-- Stolen from the HXT pickle tests. Thanks :)
pickleUnpickleTests :: (XmlPickler p, Eq p, Show p) => [p] -> PU p -> String -> Test
pickleUnpickleTests input pickler desc = TestLabel ("Pickle/unpickle tests with " ++ desc) $
                                         TestList $ map makeTests input
  where
  makeTests i = TestList $
    [ TestCase $ assertEqual "pickleDoc/unpickleDoc without XML serialisation: " [i] res1

    , TestCase $ assertEqual "pickleDoc/unpickleDoc with xshow/xread: " [i] res2

    , TestCase $
      do
      res <- res4
      assertEqual "pickle/unpickle with readFromString: " [i] res

    , TestCase $ res5 >>= assertEqual "pickle/unpickle with writeDocument/readDocument: " [i]

    , TestCase $ res6 >>= assertEqual "pickle/unpickle with xpickleDocument/xunpickleDocument: " [i]
{-
FIXME TH 15.01.2008: See below
    , TestCase $
      res7 >>= 
      assertEqual "pickle/unpickle with DTD validation xpickleDocument/xunpickleDocument: " [i]
-}
    ]
    where
    res1 = maybeToList . unpickleDoc pickler . pickleDoc pickler $ i
  
    res2 = runLA (xshow (arr (pickleDoc pickler) 
             >>> getChildren)
             >>> root [] [xread]
             >>> arrL (maybeToList . unpickleDoc pickler)) i
  
    res4 = runX (constA i
            >>> arr (pickleDoc pickler)                   -- Inverted => XmlTree
            >>> writeDocumentToString []                  -- XmlTree => String
            >>> readFromString [(a_validate, v_0)]        -- String => XmlTree
            >>> arrL (maybeToList . unpickleDoc pickler)) -- XmlTree => Inverted
  
    res5 = runX (constA i                                    -- Take the Inverted value
            >>> arr (pickleDoc pickler)                      -- Inverted => XmlTree
            >>> writeDocument [(a_indent, v_1)] "data/pickle.xml" -- XmlTree => formated external XML document
            >>> readDocument  [(a_remove_whitespace, v_1), (a_validate, v_0)] "data/pickle.xml" -- Formated external XML document => XmlTree 
            >>> arrL (maybeToList . unpickleDoc pickler))    -- XmlTree => Inverted
  
    res6 = runX (constA i -- Same as above the convinient way
            >>> xpickleDocument pickler [(a_indent, v_1)] "data/pickle.xml"
            >>> xunpickleDocument pickler [(a_remove_whitespace, v_1), (a_validate, v_0)] "data/pickle.xml")

{-
FIXME TH 15.01.2008: Adding a DTD automatically does not work yet, because we use
                     the doc element twice with different meanings: Once as part of the
                     document table and as part of an index, too  
    res7 :: IO [Inverted]                                    -- Same as above with validation
    res7 = runX (constA i
            >>> xpickleDocument xpInverted [(a_indent, v_1), (a_addDTD, v_1)] "data/pickle.xml"
            >>> xunpickleDocument xpInverted [(a_remove_whitespace, v_1), (a_validate, v_1)] "data/pickle.xml")
--}

allTests :: Test  
allTests = TestList [ pickleUnpickleTests [testIndex1, testIndex2] xpickle "Inverted"
                    , pickleUnpickleTests [testResult1, testResult2] xpickle "Result"
                    , pickleUnpickleTests [testDocs1, testDocs2] xpickle "Documents"
                    ]
