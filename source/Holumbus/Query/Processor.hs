-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Processor
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Holumbus query processor. Supports exact word or phrase queries as well
  as fuzzy word and case-insensitive word and phrase queries. Boolean
  operators like AND, OR and NOT are supported. Context specifiers and
  priorities are supported, too.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Processor 
  (
  -- * Processing
  process
  )
where

import Holumbus.Query.Syntax
import Holumbus.Index.Common

import Holumbus.Query.Fuzzy (FuzzyScore)
import qualified Holumbus.Query.Fuzzy as F

import Holumbus.Query.Result (Result)
import qualified Holumbus.Query.Result as R

import Maybe

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

allDocuments :: HolIndex i => Context -> i -> Result
allDocuments c i = R.fromList c (allWords c i) -- TODO: This definitely needs optimization.

-- | Process a query on a index with a list of contexts (should default to all contexts).
process :: HolIndex i => Query -> i -> [Context] -> Result
process q i cs = processContexts cs i q

processContexts :: HolIndex i => [Context] -> i -> Query -> Result
processContexts cs i q = foldr R.union R.emptyResult (map (\c -> process' q c i) cs)

-- | Continue processing a query by deciding what to do depending on the current query element.
process' :: HolIndex i => Query -> Context -> i -> Result
process' (Word s) c i           = processWord c i s
process' (Phrase s) c i         = processPhrase c i s
process' (CaseWord s) c i       = processCaseWord c i s
process' (CasePhrase s) c i     = processCasePhrase c i s
process' (FuzzyWord s) c i      = processFuzzyWord c i s
process' (Negation q) c i       = processNegation c i q
process' (BinQuery o q1 q2) c i = processBin c i o q1 q2
process' (Specifier cs q) _ i   = processContexts cs i q           -- Context switch: start all over

-- | Process a single, case-insensitive word by finding all documents which contain the word as prefix.
processWord :: HolIndex i => Context -> i -> String -> Result
processWord c i q = R.fromList c (prefixNoCase c i q)

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.
processCaseWord :: HolIndex i => Context -> i -> String -> Result
processCaseWord c i q = R.fromList c (prefixCase c i q)

-- | Process a phrase case-insensitive.
processPhrase :: HolIndex i => Context -> i -> String -> Result
processPhrase = processPhraseInternal lookupNoCase

-- | Process a phrase case-sensitive.
processCasePhrase :: HolIndex i => Context -> i -> String -> Result
processCasePhrase = processPhraseInternal lookupCase

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: HolIndex i => (Context -> i -> String -> [Occurrences]) -> Context -> i -> String -> Result
processPhraseInternal f c i q = let
  w = words q 
  s = mergeOccurrences $ f c i (head w) in
  if s == IM.empty then R.emptyResult
  else R.Result (R.createDocHits c [(q, processPhrase' (tail w) 1 s)]) R.emptyWordHits
  where
  processPhrase' :: [String] -> Int -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (IM.filterWithKey (nextWord $ f c i x) o)
    where
    nextWord :: [Occurrences] -> Int -> Positions -> Bool
    nextWord [] _ _  = False
    nextWord no d np = maybe False hasSuccessor (IM.lookup d (mergeOccurrences no))
      where
      hasSuccessor :: Positions -> Bool
      hasSuccessor s = IS.fold (\cp r -> r || (IS.member (cp + p) s)) False np

-- | Process a single word and try some fuzzy alternatives if nothing was found.
processFuzzyWord :: HolIndex i => Context -> i -> String -> Result
processFuzzyWord c i oq = processFuzzyWord' (F.toList $ F.fuzzUntil 1.0 oq) (processWord c i oq)
  where
  processFuzzyWord' :: [ (String, FuzzyScore) ] -> Result -> Result
  processFuzzyWord' []     r = r
  processFuzzyWord' (q:qs) r = if R.null r then processFuzzyWord' qs (processWord c i (fst q)) else r

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: HolIndex i => Context -> i -> Query -> Result
processNegation c i q = R.difference (allDocuments c i) (process' q c i)

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: HolIndex i => Context -> i -> BinOp -> Query -> Query -> Result
processBin c i And q1 q2 = R.intersection (process' q1 c i) (process' q2 c i)
processBin c i Or q1 q2  = R.union (process' q1 c i) (process' q2 c i)

-- | Merge occurrences
mergeOccurrences :: [Occurrences] -> Occurrences
mergeOccurrences o = foldr (IM.unionWith IS.union) IM.empty o
