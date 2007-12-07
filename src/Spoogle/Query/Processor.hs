-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Query.Processor
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   The Spoogle query processor.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Query.Processor where

import Spoogle.Query.Parser
import Spoogle.Index.Inverted

import qualified Spoogle.Data.Patricia as P

import Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

--import Data.Set (Set)
--import qualified Data.Set as S

type Result = IntMap Completions          -- Key is document id
type Completions = Map String Positions

emptyCompletions :: Completions
emptyCompletions = M.empty

emptyResult :: Result
emptyResult = IM.empty

process :: Query -> InvIndex -> Context -> Result
process (Word s) i c = processWord s i c
process (Phrase s) i c = processPhrase s i c
process (Specifier c q) i _ = process q i c
process (Negation q) i c = processNegation q i c
process (BinQuery o q1 q2) i c = processBin o q1 q2 i c

getPart :: InvIndex -> Context -> Maybe Part
getPart i c = M.lookup c (indexParts i)

allDocuments :: InvIndex -> Context -> Result
allDocuments i c = if isNothing part then emptyResult
                   else transformOccurrences (P.toList (fromJust part))
                   where
                     part = getPart i c

-- | Process a single word by finding all documents which contain the word as prefix.
processWord :: String -> InvIndex -> Context -> Result
processWord s i c = if isNothing part then emptyResult
                    else transformOccurrences (P.prefixFindWithKey s (fromJust part))
                    where
                      part = getPart i c

processPhrase :: String -> InvIndex -> Context -> Result
processPhrase q i c = emptyResult

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: Query -> InvIndex -> Context -> Result
processNegation q i c = IM.difference (allDocuments i c)  (process q i c)

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Query -> Query -> InvIndex -> Context -> Result
processBin And q1 q2 i c = IM.intersectionWith (M.union) (process q1 i c) (process q2 i c)
processBin Or q1 q2 i c = IM.union (process q1 i c) (process q2 i c)

-- | Transforms a list of occurrences into a result, which can be better processed,
transformOccurrences :: [(String, Occurrences)] -> Result
transformOccurrences = foldr (\(s, o) r -> IM.foldWithKey (buildCompletions s) r o) emptyResult
  where
    buildCompletions :: String -> Int -> Positions -> Result -> Result
    buildCompletions k d p r = IM.insert d (addPos (IM.findWithDefault emptyCompletions d r)) r
                       where
                         addPos :: Completions -> Completions
                         addPos c = M.insert k p c

-- | Prepares a result for better processing by transformin it into a pair of documents and completions.
prepareResult :: Result -> InvIndex -> ([String], [String])
prepareResult r i = (extractDocuments, extractCompletions)
  where
    extractDocuments :: [String]
    extractDocuments = map fromJust (map (\d -> IM.lookup d (idToDoc (docTable i))) 
                             (IM.foldWithKey (\k _ l -> k:l) [] r))
    extractCompletions :: [String]
    extractCompletions = S.toList (IM.fold (\c s -> foldl (flip S.insert) s (map fst (M.toList c))) S.empty r)