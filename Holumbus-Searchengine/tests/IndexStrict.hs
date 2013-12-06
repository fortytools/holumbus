{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow         (second)
import           Control.DeepSeq       (($!!))

import           Data.Binary (Binary)
import           Data.Char (isAlpha, toLower)
import           Data.Monoid
import           Data.List(sort)

import           GHC.AssertNF

import           Holumbus.Index.Inverted.CompressedPrefixMem
import           Holumbus.Index.Common

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck as Q (arbitrary, Property)
import qualified Test.QuickCheck.Monadic as Q (assert, monadicIO, pick, run, PropertyM)
import           Test.HUnit                           hiding (Test, Testable)

default(Int)

main :: IO ()
main = defaultMain
       [
       ]

test_isNF :: Assertion 
test_isNF = fmap not (isNF [(1::Int)..10]) @? "isNF"

checkIsNF :: Show a => a -> Assertion
checkIsNF !m = isNF m @? ("isNF " ++ show m)

docToWords :: String -> [(Word, Position)]
docToWords = flip zip [0..] . words

docToContextWords :: Context -> (Word -> Word) -> Int -> String -> [(Context, Word, Occurrences)]
docToContextWords cx f i s
    = map (\(w, p) -> (cx, f w, singletonOccurrence (mkDocId . toInteger $ i) p)) ws
      where
        ws = docToWords s


indexer :: Int -> String -> [(Context, Word, Occurrences)]
indexer i s
    = concat
      [ docToContextWords "word" id      i s'
      , docToContextWords "rev"  reverse i s'
      , docToContextWords "sort" sort    i s'
      ]
    where
      s' = map (\ x -> if isAlpha x then toLower x else ' ') s

idx :: (Binary occ, ComprOccurrences occ) => (Int, String) -> Inverted occ
idx = foldl (\ ix (c, w, o) -> insertOccurrences c w o ix) emptyInverted . uncurry indexer

d0 :: (Int, String)
d0 = (0, "abc")

d1 :: (Int, String)
d1 = (100, "Alles hat ein Ende, nur die Wurst hat zwei")

d2 :: (Int, String)
d2 = (200, "Am Ende ist entscheidend, was hinten raus kommt")

d3 :: (Int, String)
d3 = (300, "Wiso, weshalb, warum, wer nicht fragt bleibt dumm")

d4 :: (Int, String)
d4 = (400, "wer, wie, was, der, die, das, einer, eine eines")

checkIdx :: Show occ => String -> Inverted occ -> Test
checkIdx s ix = testCase ("checkIsNF: " ++ s) (checkIsNF ix)
