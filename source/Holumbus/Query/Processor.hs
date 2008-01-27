-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Processor
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  The Holumbus query processor. Supports exact word or phrase queries as well
  as fuzzy word and case-insensitive word and phrase queries. Boolean
  operators like AND, OR and NOT are supported. Context specifiers and
  priorities are supported, too.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -farrows #-}

module Holumbus.Query.Processor 
  (
  -- * Processor types
  ProcessConfig (..)
  
  -- * Processing
  , processQuery
  )
where

import Data.Maybe

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Holumbus.Index.Common (HolIndex, HolDocuments, Context, Occurrences, Positions)
import qualified Holumbus.Index.Common as IDX

import Holumbus.Query.Syntax

import Holumbus.Query.Fuzzy (FuzzyScore, FuzzyConfig)
import qualified Holumbus.Query.Fuzzy as F

import Holumbus.Query.Result (Result)
import qualified Holumbus.Query.Result as R

import Holumbus.Query.Intermediate (Intermediate)
import qualified Holumbus.Query.Intermediate as I

-- | The configuration for the query processor.
data ProcessConfig = ProcessConfig { queryServers  :: ![String]
                                   , fuzzyConfig   :: !FuzzyConfig
                                   }

-- | The internal state of the query processor.
data HolIndex i => ProcessState i = ProcessState { config   :: !ProcessConfig
                                                 , contexts :: ![Context]
                                                 , index    :: !i
                                                 }

-- | Get the fuzzy config out of the process state.
getFuzzyConfig :: HolIndex i => ProcessState i -> FuzzyConfig
getFuzzyConfig = fuzzyConfig . config

-- | Set the current context in the state.
setContexts :: HolIndex i => [Context] -> ProcessState i -> ProcessState i
setContexts cs (ProcessState cfg _ i) = ProcessState cfg cs i

-- | Initialize the state of the processor.
initState :: HolIndex i => ProcessConfig -> i -> ProcessState i
initState cfg i = ProcessState cfg (IDX.contexts i) i

forAllContexts :: (Context -> Intermediate) -> [Context] -> Intermediate
forAllContexts f cs = foldr I.union I.empty $ map f cs

-- | Just everything.
allDocuments :: HolIndex i => ProcessState i -> Intermediate
allDocuments s = forAllContexts (\c -> I.fromList "" c $ IDX.allWords (index s) c) (contexts s)

-- | Process a query on a specific index with regard to the configuration. Before processing,
-- the query will be automatically optimized.
processQuery :: (HolIndex i, HolDocuments d) => ProcessConfig -> i -> d -> Query -> Result
processQuery cfg i d q = I.toResult i d (process (initState cfg i) (optimize q))

-- | Continue processing a query by deciding what to do depending on the current query element.
process :: HolIndex i => ProcessState i -> Query -> Intermediate
process s (Word w)           = processWord s w
process s (Phrase w)         = processPhrase s w
process s (CaseWord w)       = processCaseWord s w
process s (CasePhrase w)     = processCasePhrase s w
process s (FuzzyWord w)      = processFuzzyWord s w
process s (Negation q)       = processNegation s q
process s (BinQuery o q1 q2) = processBin s o q1 q2
process s (Specifier cs q)   = process (setContexts cs s) q

-- | Process a single, case-insensitive word by finding all documents which contain the word as prefix.
processWord :: HolIndex i => ProcessState i -> String -> Intermediate
processWord s q = forAllContexts wordNoCase (contexts s)
  where
  wordNoCase c = I.fromList q c $ IDX.prefixNoCase (index s) c q

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.
processCaseWord :: HolIndex i => ProcessState i -> String -> Intermediate
processCaseWord s q = forAllContexts wordCase (contexts s)
  where
  wordCase c = I.fromList q c $ IDX.prefixCase (index s) c q

-- | Process a phrase case-insensitive.
processPhrase :: HolIndex i => ProcessState i -> String -> Intermediate
processPhrase s q = forAllContexts phraseNoCase (contexts s)
  where
  phraseNoCase c = processPhraseInternal (IDX.lookupNoCase (index s) c) c q

-- | Process a phrase case-sensitive.
processCasePhrase :: HolIndex i => ProcessState i -> String -> Intermediate
processCasePhrase s q = forAllContexts phraseCase (contexts s)
  where
  phraseCase c = processPhraseInternal (IDX.lookupCase (index s) c) c q

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: (String -> [Occurrences]) -> Context -> String -> Intermediate
processPhraseInternal f c q = let
  w = words q 
  m = mergeOccurrences $ f (head w) in
  if m == IM.empty then I.empty
  else I.fromList "" c [(q, processPhrase' (tail w) 1 m)]
  where
  processPhrase' :: [String] -> Int -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (IM.filterWithKey (nextWord $ f x) o)
    where
    nextWord :: [Occurrences] -> Int -> Positions -> Bool
    nextWord [] _ _  = False
    nextWord no d np = maybe False hasSuccessor (IM.lookup d (mergeOccurrences no))
      where
      hasSuccessor :: Positions -> Bool
      hasSuccessor w = IS.fold (\cp r -> r || (IS.member (cp + p) w)) False np

-- | Process a single word and try some fuzzy alternatives if nothing was found.
processFuzzyWord :: HolIndex i => ProcessState i -> String -> Intermediate
processFuzzyWord s oq = processFuzzyWord' (F.toList $ F.fuzz (getFuzzyConfig s) oq) (processWord s oq)
  where
  processFuzzyWord' :: [(String, FuzzyScore)] -> Intermediate -> Intermediate
  processFuzzyWord' []     r = r
  processFuzzyWord' (q:qs) r = if I.null r then processFuzzyWord' qs (processWord s (fst q)) else r

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: HolIndex i => ProcessState i -> Query -> Intermediate
processNegation s q = I.difference (allDocuments s) (process s q)

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: HolIndex i => ProcessState i -> BinOp -> Query -> Query -> Intermediate
processBin s And q1 q2 = I.intersection (process s q1) (process s q2)
processBin s Or q1 q2  = I.union (process s q1) (process s q2)
processBin s But q1 q2 = I.difference (process s q1) (process s q2)

-- | Merge occurrences
mergeOccurrences :: [Occurrences] -> Occurrences
mergeOccurrences = IM.unionsWith IS.union
