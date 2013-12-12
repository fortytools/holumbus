{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import           Control.Arrow                               (first, second,
                                                              (***))
import           Control.DeepSeq                             (($!!))
import           Control.Monad                               (when)

import           Data.Binary                                 (Binary, encodeFile, decodeFile)
import           Data.Char                                   (isAlpha, toLower)
import           Data.List                                   (sort, tails)
import           Data.Monoid

import           GHC.AssertNF

import           Holumbus.Index.Common
import           Holumbus.Index.Inverted.CompressedPrefixMem

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                                  hiding (Test,
                                                              Testable)
{- QuickCheck not yet needed
import           Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck                             as Q (Property,
                                                                   arbitrary)
import qualified Test.QuickCheck.Monadic                     as Q (PropertyM,
                                                                   assert,
                                                                   monadicIO,
                                                                   pick, run)
-}

-- ----------------------------------------

default(Int)

main :: IO ()
main = defaultMain $
       concat
       [ []
       , txs0
       , txs1
       , txs2
       , txs3
       -- , txs4	-- InvertedOserialized	-- ghc-7.6.2 bug
       ]

-- ----------------------------------------

testNF :: String -> a -> Test
testNF s !x = testCase s $
              ( do ok <- isNF x
                   when (not ok) (assertNF x)
                   return ok ) @? s

testNF' :: (a -> IO b) -> String -> a -> Test
testNF' f s y = testCase s $
              ( do !x <- f y
                   ok <- isNF x
                   when (not ok) (assertNF x)
                   return ok ) @? s

-- ----------------------------------------

docToWords :: String -> [(Word, Position)]
docToWords = flip zip [0..] . words

docToContextWords :: Context -> (Word -> Word) -> Int -> String -> [(Context, Word, Occurrences)]
docToContextWords cx f i s
    = cx' `seq` map (\(w, p) -> (cx, f w, singletonOccurrence (mkDocId . toInteger $ i) p)) ws
      where
        ws = docToWords s
        cx' = id $!! cx		-- forcing evaluation of contexts to NF

-- parse a text and build wordlists for three contexts
-- .1 the original word
-- .2 the reversed word
-- .3 word chars sorted

indexer :: Int -> String -> [(Context, Word, Occurrences)]
indexer i s
    = concat
      [ docToContextWords "word" id      i s'
      , docToContextWords "rev"  reverse i s'
      , docToContextWords "sort" sort    i s'
      ]
    where
      s' = map (\ x -> if isAlpha x then toLower x else ' ') s


-- build an index from a DocId and a text

idx :: (Binary occ, ComprOccurrences occ) => (Int, String) -> Inverted occ
idx = foldl (\ ix (c, w, o) -> insertOccurrences c w o ix) emptyInverted . uncurry indexer

-- ----------------------------------------
--
-- 5 small text docs

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

-- ----------------------------------------
--
-- all tests for 4 variants of indexes

type Ixs ix = [(String, ix)]

-- the indexes

ixs :: (Binary occ, ComprOccurrences occ) =>
       Ixs (Inverted occ)
ixs = {- -}
    ixsDocs ds				-- basic docs
    ++
    ixsMerge (ds ++ ds)  		-- merge indexes, the (++) leads to real work in unionWith
    ++
    ixsSubtract ds -- (ds ++ ds)	-- ghc-7.6.2 bug
    ++
    ixMergeSubAll (ixsDocs ds)		-- merge and subtract
    ++
    map ixIncrDocIds (ixsDocs ds)	-- modify doc ids, map test
    where
      ixsDocs =
          map (second idx)

      ixsMerge =
          ixMergeAll . ixsDocs

      ixsSubtract
          = ixSubtractAll . ixsDocs

ixs' :: (Binary occ, ComprOccurrences occ) =>
       Ixs (Inverted occ)
ixs' = map (second idx) ds

ds :: [(String, (Int, String))]
ds =
    [ ("d0", d0)
    , ("d1", d1)
    , ("d2", d2)
    , ("d3", d3)
    , ("d4", d4)
    ]

ixs0, ixs0' :: Ixs Inverted0
ixs0  = ixs
ixs0' = ixs'

ixs1, ixs1'  :: Ixs InvertedCompressed
ixs1  = ixs
ixs1' = ixs'

ixs2, ixs2'  :: Ixs InvertedSerialized
ixs2  = ixs
ixs2' = ixs'

ixs3, ixs3' :: Ixs InvertedCSerialized
ixs3  = ixs
ixs3' = ixs'

ixs4, ixs4' :: Ixs InvertedOSerialized
ixs4 = ixs
ixs4' = ixs'

-- the tests

toTestNF' :: (d -> IO a) -> String -> [(String, d)] -> [Test]
toTestNF' f name = map (uncurry (testNF' f) . first ((name ++ ": ") ++))

toTestNF :: String -> [(String, d)] -> [Test]
toTestNF = toTestNF' return

toTestBinaryNF :: (Binary occ, ComprOccurrences occ) => String -> [(String, Inverted occ)] -> [Test]
toTestBinaryNF = toTestNF' serialize

serialize :: (Binary a, ComprOccurrences a) => Inverted a -> IO (Inverted a)
serialize x = do encodeFile "/tmp/x.bin" x
                 y <- decodeFile "/tmp/x.bin"
                 return (y `mergeIndexes` idx d4)

txs0 :: [Test]
txs0 = toTestNF "Inverted0" ixs0
       ++
       toTestBinaryNF "Inverted0 (Binary)" ixs0'

txs1 :: [Test]
txs1 = toTestNF "InvertedCompressed" ixs1
       ++
       toTestBinaryNF "InvertedCompressed (Binary)" ixs1'

txs2 :: [Test]
txs2 = toTestNF "InvertedSerialized" ixs2

txs3 :: [Test]
txs3 = toTestNF "InvertedCSerialized" ixs3

txs4 :: [Test]
txs4 = toTestNF "InvertedOSerialized" ixs4

-- ----------------------------------------

ixMerge :: HolIndex t => (String, t) -> (String, t) -> (String, t)
ixMerge (s1, ix1) (s2, ix2)
    = ( unwords["(" ++ s1, "`mergeIndexes`", s2 ++ ")"]
      , ix1 `mergeIndexes` ix2
      )

ixMergeSeq :: HolIndex t => [(String, t)] -> (String, t)
ixMergeSeq
    = foldr1 ixMerge

ixMergeAll :: HolIndex t => [(String, t)] -> [(String, t)]
ixMergeAll
    = map ixMergeSeq . reverse . drop 2 . reverse . tails

-- ----------------------------------------

ixSubtract :: HolIndex t => (String, t) -> (String, t) -> (String, t)
ixSubtract (s1, ix1) (s2, ix2)
    = ( unwords["(" ++ s1, "`substractIndexes`" ,s2 ++ ")"]
      , ix1 `substractIndexes` ix2
      )

ixSubtractSeq :: HolIndex t => [(String, t)] -> (String, t)
ixSubtractSeq
    = foldl1 ixSubtract

ixSubtractAll :: HolIndex t => [(String, t)] -> [(String, t)]
ixSubtractAll
    = map ixSubtractSeq . reverse . drop 2 . reverse . tails

ixMergeSubAll :: HolIndex t => [(String, t)] -> [(String, t)]
ixMergeSubAll xs
    = ixSubtractAll (ixMergeSeq xs : xs)

-- ----------------------------------------

ixUpdateDocIds :: HolIndex t =>
                  String -> (Context -> Word -> DocId -> DocId) -> (String, t) -> (String, t)
ixUpdateDocIds n f (s, ix)
    = ( unwords ["updateDocIds", n, s]
      , updateDocIds f ix
      )

ixIncrDocIds :: HolIndex t =>
                (String, t) -> (String, t)
ixIncrDocIds
    = ixUpdateDocIds "(1+)" (const $ const incrDocId)

-- ----------------------------------------



