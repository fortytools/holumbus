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

import Data.Binary

import Test.HUnit

import SampleData

import Control.Concurrent

testIndex1, testIndex2 :: InvIndex
testIndex1 = empty
testIndex2 = sampleIndex1

binaryTests :: [InvIndex] -> String -> Test
binaryTests input desc = TestLabel ("Binary encode/decode tests with " ++ desc) $
                                         TestList $ map makeTests input
  where
  makeTests i = TestList $ 
    [ TestCase $ assertEqual "encode/decode without writing to file" i res1
    , TestCase $ res2 >>= assertEqual "encode/decode with writing to file" i
    , TestCase $ res3 >>= assertEqual "encode/decode with writing to XML file" i 
    ]
    where
    res1 = decode . encode $ i
    res2 = do
           writeToBinFile "data/binary1.bin" i
           threadDelay 1000 -- Avoid start reading the file before it is written
           loadFromBinFile "data/binary1.bin"
    res3 = do
           writeToBinFile "data/binary2.bin" i
           threadDelay 1000 -- Avoid start reading the file before it is written
           bi <- loadFromBinFile "data/binary2.bin"
           writeToXmlFile "data/binary1.xml" bi
           xi <- loadFromXmlFile "data/binary1.xml"
           writeToBinFile "data/binary3.bin" xi
           threadDelay 1000 -- Avoid start reading the file before it is written
           loadFromBinFile "data/binary3.bin"

allTests :: Test  
allTests = TestLabel "StrMap tests" $ 
  TestList
  [ binaryTests [testIndex1, testIndex2] "InvIndex"
  ]
