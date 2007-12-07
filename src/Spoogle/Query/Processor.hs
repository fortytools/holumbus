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

module Spoogle.Query.Processor (Result (Res, hits, hints), Hints, Hits, Completions, process) where

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

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

data Result = Res { hits :: !Hits, hints :: !Hints}

type Hints = Set String
type Hits = IntMap Completions          -- Key is document id
type Completions = Map String Positions

emptyHits :: Hits
emptyHits = IM.empty

emptyHints :: Hints
emptyHints = S.empty

emptyCompletions :: Completions
emptyCompletions = M.empty

emptyResult :: Result
emptyResult = Res emptyHits emptyHints

allDocuments :: Part -> Hits
allDocuments p = genHits (P.toList p)

-- | Start processing a query by selecting the context.
process :: Query -> InvIndex -> Context -> Result
process q i c = if isNothing p then
                  emptyResult
                else
                  process' q (fromJust p) i
                where
                  p = M.lookup c (indexParts i)

-- | Continue processing a query by deciding what to do depending on the current query element.
process' :: Query -> Part -> InvIndex -> Result
process' (Word s) p _           = processWord s p
process' (Phrase s) p _         = processPhrase s p
process' (Negation q) p i       = processNegation q p i
process' (BinQuery o q1 q2) p i = processBin o q1 q2 p i
process' (Specifier c q) _ i    = process q i c           -- Context switch: start all over

-- | Process a single word by finding all documents which contain the word as prefix.
processWord :: String -> Part -> Result
processWord s p = Res (genHits r)  (genHints r)
  where
    r = P.prefixFindWithKey s p

processPhrase :: String -> Part -> Result
processPhrase _ _ = emptyResult

--processPhrase :: String -> Part -> Result
--processPhrase q p = let w = words q 
--                        s = P.find (head w) p in
--                    if isNothing s then emptyResult
--                    else Res (processPhrase' (tail w) (fromJust s) p) emptyHints
--                    where
--                      processPhrase' :: [String] -> Occurrences -> Part -> Occurrences
--                      processPhrase' [] o _ = o
--                      processPhrase' (x:xs) o p = processPhrase' xs (IM.filterWithKey (nextWord ) o) p

--nextWord :: Int -> Positions -> Maybe Occurrences -> Bool
--
--hasSuccessor :: Positions -> Positions -> Bool
--hasSuccessor p s = IS.fold (\cp r -> r || (IS.member (cp + 1) s)) False p

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: Query -> Part -> InvIndex -> Result
processNegation q p i = Res (IM.difference (allDocuments p) (hits r)) (hints r)
  where
    r = process' q p i


-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Query -> Query -> Part -> InvIndex -> Result
processBin And q1 q2 p i = Res (IM.intersectionWith (M.union) (hits r1) (hits r2)) (S.union (hints r1) (hints r2))
  where
    r1 = process' q1 p i
    r2 = process' q2 p i
processBin Or q1 q2 p i  = Res (IM.union (hits r1) (hits r2)) (S.union (hints r1) (hints r2))
  where
    r1 = process' q1 p i
    r2 = process' q2 p i

-- | Transforms a list of occurrences into a list of hits, which can be better processed.
genHits :: [(String, Occurrences)] -> Hits
genHits = foldr (\(s, o) r -> IM.foldWithKey (buildCompletions s) r o) emptyHits
  where
    buildCompletions :: String -> Int -> Positions -> Hits -> Hits
    buildCompletions k d p r = IM.insert d (addPos (IM.findWithDefault emptyCompletions d r)) r
                       where
                         addPos :: Completions -> Completions
                         addPos c = M.insert k p c

-- | Transforms a list of occurrences into a list of hints, which can be better processes.
genHints :: [(String, Occurrences)] -> Hints
genHints = foldr (\(s, _) r -> S.insert s r) emptyHints

---- | Prepares a result for better processing by transformin it into a pair of documents and completions.
--prepareResult :: Result -> InvIndex -> ([String], [String])
--prepareResult r i = (extractDocuments, extractCompletions)
--  where
--    extractDocuments :: [String]
--    extractDocuments = map fromJust (map (\d -> IM.lookup d (idToDoc (docTable i))) 
--                             (IM.foldWithKey (\k _ l -> k:l) [] r))
--    extractCompletions :: [String]
--    extractCompletions = S.toList (IM.fold (\c s -> foldl (flip S.insert) s (map fst (M.toList c))) S.empty r)