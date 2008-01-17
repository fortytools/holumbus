-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Result
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

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
  , WordInfo (..)
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
  , sizeDocHits
  , sizeWordHits
  , maxScoreDocHits
  , maxScoreWordHits

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
  , setDocScore
  , setWordScore
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

import Holumbus.Control.Sequence

import Holumbus.Index.Common hiding (sizeDocs, sizeWords)
import Holumbus.Index.Combined
import Holumbus.Index.Documents

-- | The combined result type for Holumbus queries.
data Result = Result        { docHits  :: !DocHits
                            , wordHits :: !WordHits
                            }
                            deriving (Eq, Show)

-- | Information about an document, either just the document id or the whole document information.
data DocInfo = DocInfo { document :: !Document
                       , docScore :: !Score
                       }
                       deriving (Eq, Show)

-- | Information about a word.
data WordInfo = WordInfo { term      :: ![String]
                         , wordScore :: !Score 
                         }
                         deriving (Eq, Show)

-- | A mapping from a document to it's score and the contexts where it was found.
type DocHits = IntMap (DocInfo, DocContextHits)
-- | A mapping from a context to the words of the document that were found in this context.
type DocContextHits = Map Context DocWordHits
-- | A mapping from a word of the document in a specific context to it's positions.
type DocWordHits = Map Word Positions

-- | A mapping from a word to it's score and the contexts where it was found.
type WordHits = Map Word (WordInfo, WordContextHits)
-- | A mapping from a context to the documents that contain the word that were found in this context.
type WordContextHits = Map Context WordDocHits
-- | A mapping from a document containing the word to the positions of the word.
type WordDocHits = Occurrences -- IntMap Positions (docId -> positions)

-- | The score of a hit (either a document hit or a word hit).
type Score = Float

instance XmlPickler Result where
  xpickle =  xpWrap (\(dh, wh) -> Result dh wh, \(Result dh wh) -> (dh, wh)) (xpPair xpDocHits xpWordHits)

instance XmlPickler DocInfo where
  xpickle = xpWrap (\(d, s) -> DocInfo d s, \(DocInfo d s) -> (d, s)) xpDocInfo'
    where
    xpDocInfo' = xpPair (xpPair (xpAttr "title" xpText0) (xpAttr "href" xpText0)) (xpAttr "score" xpPrim)

instance XmlPickler WordInfo where
  xpickle = xpWrap (\(t, s) -> WordInfo t s, \(WordInfo t s) -> (t, s)) xpWordInfo
    where
    xpWordInfo = xpPair (xpAttr "term" xpTerms) (xpAttr "score" xpPrim)
    xpTerms = xpWrap (split ",", join ",") xpText0

instance DeepSeq Result where
  deepSeq (Result dh wh) b = deepSeq dh $ deepSeq wh b

instance DeepSeq DocInfo where
  deepSeq (DocInfo d s) b = deepSeq d $ deepSeq s b

instance DeepSeq WordInfo where
  deepSeq (WordInfo s t) b = deepSeq s $ deepSeq t b

-- | The XML pickler for the result type.
xpResult :: PU Result
xpResult = xpElem "result" xpickle

-- | The XML pickler for the document hits. Will be sorted by score.
xpDocHits :: PU DocHits
xpDocHits = xpElem "dochits" $ xpWrap (IM.fromList, toListSorted) (xpList xpDocHit)
  where
  toListSorted = L.sortBy (compare `on` (docScore . fst . snd)) . IM.toList -- Sort by score
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
xpWordHits = xpElem "wordhits" $ xpWrap (M.fromList, toListSorted) (xpList xpWordHit)
  where
  toListSorted = L.sortBy (compare `on` fst) . M.toList -- Sort by word
  xpWordHit = xpElem "word" (xpPair (xpAttr "w" xpText) (xpPair xpickle xpWordContextHits))

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
size = sizeDocHits

-- | Query the number of documents in a result.
sizeDocHits :: Result -> Int
sizeDocHits = IM.size . docHits

-- | Query the number of documents in a result.
sizeWordHits :: Result -> Int
sizeWordHits = M.size . wordHits

-- | Query the maximum score of the documents.
maxScoreDocHits :: Result -> Score
maxScoreDocHits = (IM.fold (\(di, _) r -> max (docScore di) r) 0.0) . docHits

-- | Query the maximum score of the words.
maxScoreWordHits :: Result -> Score
maxScoreWordHits = (M.fold (\(wi, _) r -> max (wordScore wi) r) 0.0) . wordHits

-- | Test if the result contains anything.
null :: Result -> Bool
null = IM.null . docHits

-- | Create the document hits structure for the results from a single context.
createDocHits :: Context -> [(String, Occurrences)] -> DocHits
createDocHits c os = IM.unionsWith combineDocHits (map createDocHits' os)
  where
  createDocHits' (w, o) = IM.map (\p -> (DocInfo ("", "") 0.0, M.singleton c (M.singleton w p))) o

-- | Create the word hits structure for the results from a single context.
createWordHits :: Word -> Context -> [(String, Occurrences)] -> WordHits
createWordHits t c os = foldr insertWordHit M.empty os
  where
  insertWordHit (w, o) = M.insertWith combineWordHits w (WordInfo [t] 0.0, M.singleton c o)

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
combineWordHits :: (WordInfo, WordContextHits) -> (WordInfo, WordContextHits) -> (WordInfo, WordContextHits)
combineWordHits (i1, c1) (i2, c2) = (unionWordInfo i1 i2, M.unionWith (IM.unionWith IS.union) c1 c2)

-- | Combine two doc informations.
unionDocInfo :: DocInfo -> DocInfo -> DocInfo
unionDocInfo (DocInfo d1 s1) (DocInfo _ s2) = DocInfo d1 (unionScore s1 s2)

-- | Combine two word informations.
unionWordInfo :: WordInfo -> WordInfo -> WordInfo
unionWordInfo (WordInfo t1 s1) (WordInfo t2 s2) = WordInfo (t1 ++ t2) (unionScore s1 s2)

-- | Combine two scores (just average between them).
unionScore :: Score -> Score -> Score
unionScore = flip flip 2.0 . ((/) .) . (+)

-- | Create a result from a list of tuples.
fromList :: Word -> Context -> [(String, Occurrences)] -> Result
fromList t c xs = Result (createDocHits c xs) (createWordHits t c xs)

-- | Transform a result to a verbose result by by looking up all document information in
-- the document table of the provided index.
annotateResult :: AnyIndex -> Result -> Result
annotateResult i (Result dh wh) = Result (IM.mapWithKey convertDocInfo dh) wh
  where
  convertDocInfo docId ((DocInfo _ s), dch) = (DocInfo doc s, dch)
    where
    doc = fromJust $ lookupId docId (documents i)

-- | Set the score in a document info.
setDocScore :: Score -> DocInfo -> DocInfo
setDocScore s (DocInfo d _) = DocInfo d s

-- | Set the score in a word info.
setWordScore :: Score -> WordInfo -> WordInfo
setWordScore s (WordInfo t _) = WordInfo t s

-- | Split a string into seperate strings at a specific character.
split :: Eq a => [a] -> [a] -> [[a]]
split _ []       = [[]] 
split at w@(x:xs) = maybe ((x:r):rs) ((:) [] . split at) (stripPrefix at w)
                    where (r:rs) = split at xs
 
-- | Join with a seperating character.
join :: Eq a => [a] -> [[a]] -> [a]
join = intercalate

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.List)
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)
  where
  intersperse _   []      = []
  intersperse _   [y]     = [y]
  intersperse sep (y:ys)  = y : sep : intersperse sep ys
  
-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.List)
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing