-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Result
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The data type for results of Holumbus queries.

  The result of a query is defined in terms of two partial results, 
  the documents containing the search terms and the words which 
  are possible completions of the serach terms.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Result 
  (
  -- * Result data types
  Result (..)
  , DocHits
  , DocContextHits
  , DocWordHits
  , WordHits
  , WordContextHits
  , WordDocHits
  , DocInfo (..)
  , Score
  
  -- * Construction
  , emptyResult
  , emptyDocHits
  , emptyWordHits
  , fromList  
  , createDocHits
  , createWordHits

  -- * Query
  , size
  , null
  , sizeDocs
  , sizeWords
  , maxScoreDocs
  , maxScoreWords

  -- * Combine
  , union
  , difference
  , intersection
  
  -- * Pickling
  , xpResult
  , xpDocHits
  , xpWordHits
  
  -- * Transform
  , annotateResult
  , setTitle
  , setURI
  , setScore
  )
where

import Prelude hiding (null)

import qualified Data.List as L

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

import Text.XML.HXT.Arrow.Pickle

import Holumbus.Index.Common hiding (sizeDocs, sizeWords)
import Holumbus.Index.Documents

-- | The combined result type for Holumbus queries.
data Result = Result        { docHits  :: !DocHits
                            , wordHits :: !WordHits
                            }
            deriving (Show)

-- | Information about an document, either just the document id or the whole document information.
data DocInfo = DocInfo        { score :: !Score }
             | VerboseDocInfo { title :: !Title
                              , uri   :: !URI
                              , score :: !Score
                              }
             deriving (Show)

-- | A mapping from a document to it's score and the contexts where it was found.
type DocHits = IntMap (DocInfo, DocContextHits)
-- | A mapping from a context to the words of the document that were found in this context.
type DocContextHits = Map Context DocWordHits
-- | A mapping from a word of the document in a specific context to it's positions.
type DocWordHits = Map String Positions

-- | A mapping from a word to it's score and the contexts where it was found.
type WordHits = Map String (Score, WordContextHits)
-- | A mapping from a context to the documents that contain the word that were found in this context.
type WordContextHits = Map Context WordDocHits
-- | A mapping from a document containing the word to the positions of the word.
type WordDocHits = Occurrences

-- | The score of a hit (either a document hit or a word hit).
type Score = Float

instance XmlPickler Result where
  xpickle =  xpWrap (\(dh, wh) -> Result dh wh, \(Result dh wh) -> (dh, wh)) (xpPair xpDocHits xpWordHits)

instance XmlPickler DocInfo where
  xpickle = xpAlt tag ps
    where
    tag (DocInfo _) = 0
    tag (VerboseDocInfo _ _ _) = 1
    ps = [ xpDocInfo, xpVerboseDocInfo ]
      where
      xpDocInfo = xpWrap (DocInfo, \(DocInfo s) -> s) (xpAttr "score" xpPrim)
      xpVerboseDocInfo = xpWrap (\(t, u, s) -> VerboseDocInfo t u s, \(VerboseDocInfo t u s) -> (t, u, s)) xpVerboseDocInfo'
        where
        xpVerboseDocInfo' = xpTriple (xpAttr "title" xpText) (xpAttr "href" xpText) (xpAttr "score" xpPrim)
         
-- | The XML pickler for the result type.
xpResult :: PU Result
xpResult = xpElem "result" xpickle

-- | The XML pickler for the document hits. Will be sorted by score.
xpDocHits :: PU DocHits
xpDocHits = xpElem "dochits" $ xpWrap (IM.fromList, toListSorted) (xpList xpDocHit)
  where
  toListSorted = L.sortBy (compare `on` (score . fst . snd)) . IM.toList -- Sort by score
  xpDocHit = xpElem "doc" (xpPair (xpAttr "idref" xpPrim) (xpPair xpickle xpDocContextHits))

-- | The XML pickler for the contexts in which the documents were found.
xpDocContextHits :: PU DocContextHits
xpDocContextHits = xpWrap (M.fromList, M.toList) (xpList xpDocContextHit)
  where
  xpDocContextHit = xpElem "context" (xpPair (xpAttr "name" xpText) xpDocWordHits)

-- | The XML pickler for the words and positions found in a document.
xpDocWordHits :: PU DocWordHits
xpDocWordHits = xpWrap (M.fromList, M.toList) (xpList xpDocWordHit)
  where
  xpDocWordHit = xpElem "word" (xpPair (xpAttr "w" xpText) xpPositions)

-- | The XML pickler for the word hits. Will be sorted alphabetically by the words.
xpWordHits :: PU WordHits
xpWordHits = xpElem "wordhits" $ xpWrap (M.fromList, toListSorted) (xpList xpScoredWord)
  where
  toListSorted = L.sortBy (compare `on` fst) . M.toList -- Sort by word
  xpScoredWord = xpElem "word" (xpPair (xpAttr "w" xpText) (xpPair (xpAttr "score" xpPrim) xpWordContextHits))

-- | The XML pickler for the contexts in which the words were found.
xpWordContextHits :: PU WordContextHits
xpWordContextHits = xpWrap (M.fromList, M.toList) (xpList xpWordContextHit)
  where
  xpWordContextHit = xpElem "context" (xpPair (xpAttr "name" xpText) xpWordDocHits)

-- | The XML pickler for the documents and positions where the word occurs (reusing existing pickler).
xpWordDocHits :: PU WordDocHits
xpWordDocHits = xpOccurrences

-- | Create an empty result.
emptyResult :: Result
emptyResult = Result IM.empty M.empty

-- | Create an empty set of document hits.
emptyDocHits :: DocHits
emptyDocHits = IM.empty

-- | Create an empty set of word hits.
emptyWordHits :: WordHits
emptyWordHits = M.empty

-- | Query the size of a result.
size :: Result -> Int
size = sizeDocs

-- | Query the number of documents in a result.
sizeDocs :: Result -> Int
sizeDocs = IM.size . docHits

-- | Query the number of documents in a result.
sizeWords :: Result -> Int
sizeWords = M.size . wordHits

-- | Query the maximum score of the documents.
maxScoreDocs :: Result -> Score
maxScoreDocs = (IM.fold (\(di, _) r -> max (score di) r) 0.0) . docHits

-- | Query the maximum score of the words.
maxScoreWords :: Result -> Score
maxScoreWords = (M.fold (\(s, _) r -> max s r) 0.0) . wordHits

-- | Test if the result contains anything.
null :: Result -> Bool
null = IM.null . docHits

-- | Set the title of a document info.
setTitle :: Title -> DocInfo -> DocInfo
setTitle t (DocInfo s) = VerboseDocInfo t "" s
setTitle t (VerboseDocInfo _ u s) = VerboseDocInfo t u s

-- | Set the URI of a document info.
setURI :: URI -> DocInfo -> DocInfo
setURI u (DocInfo s) = VerboseDocInfo "" u s
setURI u (VerboseDocInfo t _ s) = VerboseDocInfo t u s

-- | Set the score of a document info.
setScore :: Score -> DocInfo -> DocInfo
setScore s (DocInfo _) = DocInfo s
setScore s (VerboseDocInfo t u _) = VerboseDocInfo t u s

-- | Create the document hits structure for the results from a single context.
createDocHits :: Context -> [(String, Occurrences)] -> DocHits
createDocHits c = foldr (\(s, o) r -> IM.foldWithKey (insertDocHit c s) r o) emptyDocHits

-- | Inserts the positions of a word in a document from a context into the result.
insertDocHit :: Context -> String -> Int -> Positions -> DocHits -> DocHits
insertDocHit c w d p r = IM.insert d (DocInfo 0.0, (M.insert c (M.insertWith IS.union w p dwh) dch)) r
  where
  dch = snd $ IM.findWithDefault (DocInfo 0.0, M.empty) d r
  dwh = M.findWithDefault M.empty c dch

-- | Create the word hits structure for the results from a single context.
createWordHits :: Context -> [(String, Occurrences)] -> WordHits
createWordHits c = foldr (\(s, o) r -> IM.foldWithKey (insertWordHit c s) r o) emptyWordHits

-- | Inserts the positions of a word in a document from a context into the result.
insertWordHit :: Context -> String -> Int -> Positions -> WordHits -> WordHits
insertWordHit c w d p r = M.insert w (0.0, (M.insert c (IM.insertWith IS.union d p wdh) wch)) r
  where
  wch = snd $ M.findWithDefault (0.0, M.empty) w r
  wdh = M.findWithDefault IM.empty c wch

-- | Combine two results by calculating their union.
union :: Result -> Result -> Result
union (Result d1 w1) (Result d2 w2) = Result (unionDocHits d1 d2) (unionWordHits w1 w2)

-- | Combine two results by calculating their intersection.
intersection :: Result -> Result -> Result
intersection (Result d1 w1) (Result d2 w2) = Result (intersectDocHits d1 d2) (unionWordHits w1 w2)

-- | Combine two results by calculating their difference.
difference :: Result -> Result -> Result
difference (Result d1 _) (Result d2 w2) = Result (IM.difference d1 d2) w2

-- | Intersect two sets of document hits.
intersectDocHits :: DocHits -> DocHits -> DocHits
intersectDocHits = IM.intersectionWith combineDocHits

-- | Combine two sets of document hits.
unionDocHits :: DocHits -> DocHits -> DocHits
unionDocHits = IM.unionWith combineDocHits

-- | Combine two sets of word hits.
unionWordHits :: WordHits -> WordHits -> WordHits
unionWordHits = M.unionWith combineWordHits

-- | Combine two tuples with score and context hits.
combineDocHits :: (DocInfo, DocContextHits) -> (DocInfo, DocContextHits) -> (DocInfo, DocContextHits)
combineDocHits (i1, c1) (i2, c2) = (unionDocInfo i1 i2, M.unionWith (M.unionWith IS.union) c1 c2)

-- | Combine two tuples with score and context hits.
combineWordHits :: (Score, WordContextHits) -> (Score, WordContextHits) -> (Score, WordContextHits)
combineWordHits (s1, c1) (s2, c2) = (unionScore s1 s2, M.unionWith (IM.unionWith IS.union) c1 c2)

-- | Combine two doc informations.
unionDocInfo :: DocInfo -> DocInfo -> DocInfo
unionDocInfo (DocInfo s1) (DocInfo s2) = DocInfo (unionScore s1 s2)
unionDocInfo (VerboseDocInfo t u s1) (DocInfo s2) = VerboseDocInfo t u (unionScore s1 s2)
unionDocInfo (DocInfo s1) (VerboseDocInfo t u s2) = VerboseDocInfo t u (unionScore s1 s2)
unionDocInfo (VerboseDocInfo t u s1) (VerboseDocInfo _ _ s2) = VerboseDocInfo t u (unionScore s1 s2)

-- | Combine two scores (just average between them).
unionScore :: Score -> Score -> Score
unionScore = flip flip 2.0 . ((/) .) . (+)

-- | Create a result from a list of tuples.
fromList :: Context -> [(String, Occurrences)] -> Result
fromList c xs = Result (createDocHits c xs) (createWordHits c xs)

-- | Transform a result to a verbose result by by looking up all document information in
-- the document table of the provided index.
annotateResult :: HolIndex i => i -> Result -> Result
annotateResult i (Result dh wh) = Result (IM.mapWithKey convertDocInfo dh) wh
  where
  convertDocInfo _ ((VerboseDocInfo t u s), dch) = (VerboseDocInfo t u s, dch)
  convertDocInfo docId ((DocInfo s), dch) = (VerboseDocInfo (fst doc) (snd doc) s, dch)
    where
    doc = fromJust $ lookupId docId (documents i)
 
-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
