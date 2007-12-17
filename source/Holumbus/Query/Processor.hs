-- ----------------------------------------------------------------------------

{- |
  Module     : Spoogle.Query.Processor
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Spoogle query processor.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Query.Processor 
  (
  -- * Result data types
  Result (hits, hints), 
  Hints, 
  Hits, 
  Completions, 

  -- * Processing
  process
  )
where

import Spoogle.Query.Parser
import Spoogle.Index.Inverted

import qualified Spoogle.Data.StrMap as SM

import Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

data Result = Res { hits :: !Hits, hints :: !Hints}

type Hints = Map String Occurrences
type Hits = IntMap Completions          -- Key is document id
type Completions = Map String Positions

emptyHits :: Hits
emptyHits = IM.empty

emptyHints :: Hints
emptyHints = M.empty

emptyCompletions :: Completions
emptyCompletions = M.empty

emptyResult :: Result
emptyResult = Res emptyHits emptyHints

allDocuments :: Part -> Hits
allDocuments p = genHits (SM.toList p)

-- | Process a query on a index with a default context.
process :: Query -> InvIndex -> Context -> Result
process q i c = maybe emptyResult ((flip (process' q)) i) p
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
    r = SM.prefixFindCaseWithKey s p

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhrase :: String -> Part -> Result
processPhrase q p = let w = words q 
                        s = SM.lookup (head w) p in
                    if isNothing s then emptyResult
                    else Res (genHits [(q, processPhrase' (tail w) 1 (fromJust s))]) emptyHints
  where
    processPhrase' :: [String] -> Int -> Occurrences -> Occurrences
    processPhrase' [] _ o = o
    processPhrase' (x:xs) i o = processPhrase' xs (i+1) (IM.filterWithKey (nextWord $ SM.lookup x p) o)
      where
        nextWord :: Maybe Occurrences -> Int -> Positions -> Bool
        nextWord Nothing   _ _  = False
        nextWord (Just no) d np = maybe False (hasSuccessor np i) (IM.lookup d no)

-- | Returns true if the second set contains any value of the first set incremented by the integer.
hasSuccessor :: Positions -> Int -> Positions -> Bool
hasSuccessor p i s = IS.fold (\cp r -> r || (IS.member (cp + i) s)) False p

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: Query -> Part -> InvIndex -> Result
processNegation q p i = Res (IM.difference (allDocuments p) (hits r)) (hints r)
  where
    r = process' q p i

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Query -> Query -> Part -> InvIndex -> Result
processBin And q1 q2 p i = Res (IM.intersectionWith M.union (hits r1) (hits r2)) (M.unionWith (IM.union) (hints r1) (hints r2))
  where
    r1 = process' q1 p i
    r2 = process' q2 p i
processBin Or q1 q2 p i  = Res (IM.union (hits r1) (hits r2)) (M.unionWith (IM.union) (hints r1) (hints r2))
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
genHints = foldr (\(s, o) r -> M.insert s o r) emptyHints
