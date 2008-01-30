-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Result
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.4

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

  -- * Query
  , null
  , sizeDocHits
  , sizeWordHits
  , maxScoreDocHits
  , maxScoreWordHits

  -- * Combine
  , merge
  
  -- * Transform
  , setDocScore
  , setWordScore
  
  -- * Picklers
  , xpDocHits
  , xpWordHits
  )
where

import Prelude hiding (null)

import Data.Binary (Binary (..))
import Control.Monad (liftM2)

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.List as L

import Control.Parallel.Strategies

import Text.XML.HXT.Arrow

import Holumbus.Index.Common

-- | The combined result type for Holumbus queries.
data Result = Result        
  { docHits  :: !DocHits   -- ^ The documents matching the query.
  , wordHits :: !WordHits  -- ^ The words which are completions of the query terms.
  }
  deriving (Eq, Show)

-- | Information about an document, either just the document id or the whole document information.
data DocInfo = DocInfo 
  { document :: !Document  -- ^ The document itself (title and URI).
  , docScore :: !Score     -- ^ The score for the document (initial score for all documents is @0.0@).
  }
  deriving (Eq, Show)

-- | Information about a word.
data WordInfo = WordInfo 
  { terms     :: ![String] -- ^ The query terms that can be extended by this word.
  , wordScore :: !Score    -- ^ The score for the word (initial score for all words is @0.0@).
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

instance Binary Result where
  put (Result dh wh) = put dh >> put wh
  get = liftM2 Result get get

instance Binary DocInfo where
  put (DocInfo d s) = put d >> put s
  get = liftM2 DocInfo get get

instance Binary WordInfo where
  put (WordInfo t s) = put t >> put s
  get = liftM2 WordInfo get get

instance NFData Result where
  rnf (Result dh wh) = rnf dh `seq` rnf wh

instance NFData DocInfo where
  rnf (DocInfo d s) = rnf d `seq` rnf s

instance NFData WordInfo where
  rnf (WordInfo t s) = rnf t `seq` rnf s

instance XmlPickler Result where
  xpickle = xpElem "result" $ 
            xpWrap (\(dh, wh) -> Result dh wh, \(Result dh wh) -> (dh, wh)) (xpPair xpDocHits xpWordHits)

instance XmlPickler DocInfo where
  xpickle = xpWrap (\(d, s) -> DocInfo d s, \(DocInfo d s) -> (d, s)) xpDocInfo'
    where
    xpDocInfo' = xpPair (xpPair (xpAttr "title" xpText0) (xpAttr "href" xpText0)) (xpAttr "score" xpPrim)

instance XmlPickler WordInfo where
  xpickle = xpWrap (\(t, s) -> WordInfo t s, \(WordInfo t s) -> (t, s)) xpWordInfo
    where
    xpWordInfo = xpPair (xpAttr "term" xpTerms) (xpAttr "score" xpPrim)
    xpTerms = xpWrap (split ",", join ",") xpText0

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

-- | Set the score in a document info.
setDocScore :: Score -> DocInfo -> DocInfo
setDocScore s (DocInfo d _) = DocInfo d s

-- | Set the score in a word info.
setWordScore :: Score -> WordInfo -> WordInfo
setWordScore s (WordInfo t _) = WordInfo t s

-- | Merge two results.
merge :: Result -> Result -> Result
merge (Result dh1 wh1) (Result dh2 wh2) = Result (mergeDocHits dh1 dh2) (mergeWordHits wh1 wh2)
  where
  mergeDocHits _ _ = IM.empty
  mergeWordHits _ _ = M.empty

-- | Split a string into seperate strings at a specific character.
split :: Eq a => [a] -> [a] -> [[a]]
split _ []       = [[]] 
split at w@(x:xs) = maybe ((x:r):rs) ((:) [] . split at) (stripPrefix at w)
                    where (r:rs) = split at xs
 
-- | Join with a seperating character.
join :: Eq a => [a] -> [[a]] -> [a]
join = intercalate

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

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
  