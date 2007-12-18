-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Processor
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Holumbus query processor.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Processor 
  (
  -- * Processing
  process
  )
where

import Holumbus.Query.Parser
import Holumbus.Index.Common
import Holumbus.Index.Inverted

import Holumbus.Query.Result (Result)
import qualified Holumbus.Query.Result as R

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Maybe

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

allDocuments :: Context -> InvIndex -> Result
allDocuments c i = R.fromList c (SM.toList $ getPart c i)

getPart :: Context -> InvIndex -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | Process a query on a index with a default context.
process :: Query -> InvIndex -> [Context] -> Result
process q i cs = processContexts cs i q

processContexts :: [Context] -> InvIndex -> Query -> Result
processContexts cs i q = foldr R.union R.emptyResult (map (\c -> process' q c i) cs)

-- | Continue processing a query by deciding what to do depending on the current query element.
process' :: Query -> Context -> InvIndex -> Result
process' (Word s) c i           = processWord c i s
process' (Phrase s) c i         = processPhrase c i s
process' (CaseWord s) c i       = processCaseWord c i s
process' (CasePhrase s) c i     = processCasePhrase c i s
process' (Negation q) c i       = processNegation c i q
process' (BinQuery o q1 q2) c i = processBin c i o q1 q2
process' (Specifier cs q) _ i    = processContexts cs i q           -- Context switch: start all over

-- | Process a single, case-insensitive word by finding all documents which contain the word as prefix.
processWord :: Context -> InvIndex -> String -> Result
processWord c i q = R.fromList c . SM.prefixFindNoCaseWithKey q $ getPart c i

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.
processCaseWord :: Context -> InvIndex -> String -> Result
processCaseWord c i q = R.fromList c . SM.prefixFindWithKey q $ getPart c i

-- | Process a phrase case-insensitive.
processPhrase :: Context -> InvIndex -> String -> Result
processPhrase = processPhraseInternal SM.lookupNoCase

-- | Process a phrase case-sensitive.
processCasePhrase :: Context -> InvIndex -> String -> Result
processCasePhrase = processPhraseInternal (\x y -> maybeToList (SM.lookup x y))

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: (String -> StrMap Occurrences -> [Occurrences]) -> Context -> InvIndex -> String -> Result
processPhraseInternal f c i q = let
  w = words q 
  s = mergeOccurrences $ f (head w) (getPart c i) in
  if s == IM.empty then R.emptyResult
  else R.Result (R.createDocHits c [(q, processPhrase' (tail w) 1 s)]) R.emptyWordHits
  where
  processPhrase' :: [String] -> Int -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (IM.filterWithKey (nextWord $ f x (getPart c i)) o)
    where
    nextWord :: [Occurrences] -> Int -> Positions -> Bool
    nextWord [] _ _  = False
    nextWord no d np = maybe False hasSuccessor (IM.lookup d (mergeOccurrences no))
      where
      hasSuccessor :: Positions -> Bool
      hasSuccessor s = IS.fold (\cp r -> r || (IS.member (cp + p) s)) False np

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: Context -> InvIndex -> Query -> Result
processNegation c i q = R.difference (allDocuments c i) (process' q c i)

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: Context -> InvIndex -> BinOp -> Query -> Query -> Result
processBin c i And q1 q2 = R.intersection (process' q1 c i) (process' q2 c i)
processBin c i Or q1 q2  = R.union (process' q1 c i) (process' q2 c i)

-- | Merge occurrences
mergeOccurrences :: [Occurrences] -> Occurrences
mergeOccurrences o = foldr (IM.unionWith IS.union) IM.empty o
