-- ----------------------------------------------------------------------------

{- |
  Module     : BinaryTest
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the binary persistence.

-}

-- ----------------------------------------------------------------------------

module BinaryTest (allTests) where

import Holumbus.Index.Inverted
import Holumbus.Index.Documents

import Data.Binary

import Test.HUnit

import SampleData

import Control.Concurrent

import System.Directory

testIndex1, testIndex2 :: Inverted
testIndex1 = emptyInverted
testIndex2 = sampleIndex1

testDocs1, testDocs2 :: Documents Int
testDocs1 = emptyDocuments
testDocs2 = sampleDocs1

binaryTests :: (Show b, Eq b, Binary b) => [b] -> String -> Test
binaryTests input desc = TestLabel ("Binary encode/decode tests with " ++ desc) $
                                         TestList $ map makeTests input
  where
  makeTests i = TestList $ 
    [ TestCase $ assertEqual "encode/decode without writing to file" i res1
    , TestCase $ res2 >>= assertEqual "encode/decode with writing to file" i
    ]
    where
    res1 = decode . encode $ i
    res2 = do
           writeToBin "data/binary1.bin" i
           threadDelay 1000 -- Avoid start reading the file before it is written
           r <- loadFromBin "data/binary1.bin"
           removeFile "data/binary1.bin"
           return r

-- | Load from a binary file.
loadFromBin :: Binary b => FilePath -> IO b
loadFromBin f = decodeFile f

-- | Write to a binary file.
writeToBin :: Binary b => FilePath -> b -> IO ()
writeToBin =  encodeFile

allTests :: Test  
allTests = TestLabel "StrMap tests" $ 
  TestList
  [ binaryTests [testIndex1, testIndex2] "Inverted"
  , binaryTests [testDocs1, testDocs2] "Documents"
  ]
