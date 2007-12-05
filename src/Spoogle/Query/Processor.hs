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

processWord :: String -> InvIndex -> Context -> Result
processWord s i c = if isNothing part then emptyResult
                    else transformOccurrences (P.prefixFindWithKey s (fromJust part))
                    where
                      part = getPart i c

processPhrase :: String -> InvIndex -> Context -> Result
processPhrase q i c = emptyResult

processNegation :: Query -> InvIndex -> Context -> Result
processNegation q i c = emptyResult

processBin :: BinOp -> Query -> Query -> InvIndex -> Context -> Result
processBin And q1 q2 i c = IM.intersectionWith (M.union) (process q1 i c) (process q2 i c)
processBin Or q1 q2 i c = IM.union (process q1 i c) (process q2 i c)

transformOccurrences :: [(String, Occurrences)] -> Result
transformOccurrences = foldr insertOccurrences emptyResult

insertOccurrences :: (String, Occurrences) -> Result -> Result
insertOccurrences (s, o) r = IM.foldWithKey (test s) r o

test :: String -> Int -> Positions -> Result -> Result
test key doc pos res = IM.insert doc (addPos (IM.findWithDefault emptyCompletions doc res)) res
                       where
                         addPos :: Completions -> Completions
                         addPos c = M.insert key pos c

transform :: Result -> InvIndex -> ([String], [String])
transform r i = (extractDocuments r i, extractCompletions r i)

extractDocuments :: Result -> InvIndex -> [String]
extractDocuments r i = map fromJust (map (\d -> IM.lookup d (idToDoc (docTable i))) 
                         (IM.foldWithKey (\k _ l -> k:l) [] r))

extractCompletions :: Result -> InvIndex -> [String]
extractCompletions r i = S.toList (IM.fold (\c s -> foldl (flip S.insert) s (map fst (M.toList c))) S.empty r)