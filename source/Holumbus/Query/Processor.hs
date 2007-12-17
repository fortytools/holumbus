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
  -- * Result data types
  ContextResult (hits, hints), 
  WordHints, 
  DocHits, 
  WordHits, 

  -- * Processing
  process
  )
where

import Holumbus.Query.Parser
import Holumbus.Index.Inverted

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

type Result = Map Context ContextResult

data ContextResult = Res { hits :: !DocHits, hints :: !WordHints} deriving (Show)

type DocHits = IntMap WordHits          -- Key is document id
type WordHits = Map String Positions
type WordHints = Map String Occurrences

emptyDocHits :: DocHits
emptyDocHits = IM.empty

emptyWordHits :: WordHits
emptyWordHits = M.empty

emptyWordHints :: WordHints
emptyWordHints = M.empty

emptyContextResult :: ContextResult
emptyContextResult = Res emptyDocHits emptyWordHints

emptyResult :: Result
emptyResult = M.empty

allDocuments :: Part -> DocHits
allDocuments p = genHits (SM.toList p)

-- | Process a query on a index with a default context.
process :: Query -> InvIndex -> [Context] -> Result
process = processContexts

processContexts :: Query -> InvIndex -> [Context] -> Result
processContexts q i c = foldr mergeResults emptyResult (map (processContext q i) c)

processContext :: Query -> InvIndex -> Context -> Result
processContext q i c = maybe emptyResult ((flip (process' q c)) i) p
                       where
                         p = M.lookup c (indexParts i)

-- | Continue processing a query by deciding what to do depending on the current query element.
process' :: Query -> Context -> Part -> InvIndex -> Result
process' (Word s) c p _           = M.singleton c (processWord s p)
process' (Phrase s) c p _         = M.singleton c (processPhrase s p)
process' (CaseWord s) c p _       = M.singleton c (processCaseWord s p)
process' (CasePhrase s) c p _     = M.singleton c (processCasePhrase s p)
process' (Negation q) c p i       = processNegation q c p i
process' (BinQuery o q1 q2) c p i = processBin o q1 q2 c p i
process' (Specifier c q) _ _ i    = processContexts q i c           -- Context switch: start all over

-- | Process a single, case-insensitive word by finding all documents which contain the word as prefix.
processWord :: String -> Part -> ContextResult
processWord s p = Res (genHits r)  (genHints r)
  where
    r = SM.prefixFindWithKey s p

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.
processCaseWord :: String -> Part -> ContextResult
processCaseWord s p = Res (genHits r)  (genHints r)
  where
    r = SM.prefixFindWithKey s p

-- | Process a phrase case-insensitive.
processPhrase :: String -> Part -> ContextResult
processPhrase = processPhraseInternal SM.lookupNoCase

-- | Process a phrase case-sensitive.
processCasePhrase :: String -> Part -> ContextResult
processCasePhrase = processPhraseInternal (\x y -> maybeToList (SM.lookup x y))

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: (String -> StrMap Occurrences -> [Occurrences]) -> String -> Part -> ContextResult
processPhraseInternal f q p = let w = words q 
                                  s = mergeOccurrences $ f (head w) p in
                                  if s == IM.empty then emptyContextResult
                                  else Res (genHits [(q, processPhrase' (tail w) 1 s)]) emptyWordHints
  where
    processPhrase' :: [String] -> Int -> Occurrences -> Occurrences
    processPhrase' [] _ o = o
    processPhrase' (x:xs) i o = processPhrase' xs (i+1) (IM.filterWithKey (nextWord $ f x p) o)
      where
        nextWord :: [Occurrences] -> Int -> Positions -> Bool
        nextWord [] _ _  = False
        nextWord no d np = maybe False (hasSuccessor np i) (IM.lookup d (mergeOccurrences no))

-- | Returns true if the second set contains any value of the first set incremented by the integer.
hasSuccessor :: Positions -> Int -> Positions -> Bool
hasSuccessor p i s = IS.fold (\cp r -> r || (IS.member (cp + i) s)) False p

-- | Process a negation by getting all documents and substracting the result of the negated query.
processNegation :: Query -> Context -> Part -> InvIndex -> Result
processNegation q c p i = emptyResult --M.singleton c (Res (IM.difference (allDocuments p) (hits r)) (hints r))
  where
    r = process' q c p i

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Query -> Query -> Context -> Part -> InvIndex -> Result
processBin And q1 q2 c p i = emptyResult --M.singleton c (Res (IM.intersectionWith M.union (hits r1) (hits r2)) (M.unionWith (IM.union) (hints r1) (hints r2)))
  where
    r1 = process' q1 c p i
    r2 = process' q2 c p i
processBin Or q1 q2 c p i  = emptyResult --M.singleton c (Res (IM.union (hits r1) (hits r2)) (M.unionWith (IM.union) (hints r1) (hints r2)))
  where
    r1 = process' q1 c p i
    r2 = process' q2 c p i

-- | Transforms a list of occurrences into a list of hits, which can be better processed.
genHits :: [(String, Occurrences)] -> DocHits
genHits = foldr (\(s, o) r -> IM.foldWithKey (buildWordHits s) r o) emptyDocHits
  where
    buildWordHits :: String -> Int -> Positions -> DocHits -> DocHits
    buildWordHits k d p r = IM.insert d (addPos (IM.findWithDefault emptyWordHits d r)) r
                            where
                              addPos :: WordHits -> WordHits
                              addPos c = M.insert k p c

-- | Transforms a list of occurrences into a list of hints, which can be better processes.
genHints :: [(String, Occurrences)] -> WordHints
genHints = foldr (\(s, o) r -> M.insert s o r) emptyWordHints

-- | Merge occurrences
mergeOccurrences :: [Occurrences] -> Occurrences
mergeOccurrences o = foldr (IM.unionWith IS.union) IM.empty o

mergeResults :: Result -> Result -> Result
mergeResults r1 r2 = r1