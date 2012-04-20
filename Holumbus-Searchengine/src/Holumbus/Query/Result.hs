-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Result
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable

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
  , getDocuments

  -- * Transform
  , setDocScore
  , setWordScore
  
  -- * Picklers
  , xpDocHits
  , xpWordHits
  )
where

import           Prelude                hiding (null)

import           Control.DeepSeq
import           Control.Monad          ( liftM2 )

import           Data.Binary            ( Binary (..) )
import           Data.Function
import           Data.Map               ( Map )
import qualified Data.Map               as M
import qualified Data.List              as L

import           Holumbus.Utility
import           Holumbus.Index.Common

import           Text.XML.HXT.Core

-- ----------------------------------------------------------------------------

-- | The combined result type for Holumbus queries.
data Result a           = Result        
                          { docHits  :: (DocHits a)  -- ^ The documents matching the query.
                          , wordHits :: WordHits     -- ^ The words which are completions of the query terms.
                          }
                          deriving (Eq, Show)

-- | Information about an document.
data DocInfo a          = DocInfo 
                          { document :: (Document a) -- ^ The document itself.
                          , docScore :: Score        -- ^ The score for the document (initial score for all documents is @0.0@).
                          }
                          deriving (Eq, Show)

-- | Information about a word.
data WordInfo           = WordInfo 
                          { terms     :: Terms    -- ^ The search terms that led to this very word.
                          , wordScore :: Score    -- ^ The frequency of the word in the document for a context.
                          }
                          deriving (Eq, Show)

-- | A mapping from a document to it's score and the contexts where it was found.
type DocHits a          = DocIdMap (DocInfo a, DocContextHits)

-- | A mapping from a context to the words of the document that were found in this context.
type DocContextHits     = Map Context DocWordHits

-- | A mapping from a word of the document in a specific context to it's positions.
type DocWordHits        = Map Word Positions

-- | A mapping from a word to it's score and the contexts where it was found.
type WordHits           = Map Word (WordInfo, WordContextHits)

-- | A mapping from a context to the documents that contain the word that were found in this context.
type WordContextHits    = Map Context WordDocHits

-- | A mapping from a document containing the word to the positions of the word.
type WordDocHits        = Occurrences

-- | The score of a hit (either a document hit or a word hit).
type Score              = Float

-- | The original search terms entered by the user.
type Terms              = [String]

-- ----------------------------------------------------------------------------

instance Binary a => Binary (Result a) where
  put (Result dh wh)    = put dh >> put wh
  get                   = liftM2 Result get get

instance Binary a => Binary (DocInfo a) where
  put (DocInfo d s)     = put d >> put s
  get                   = liftM2 DocInfo get get

instance Binary WordInfo where
  put (WordInfo t s)    = put t >> put s
  get                   = liftM2 WordInfo get get

instance NFData a => NFData (Result a) where
  rnf (Result dh wh)    = rnf dh `seq` rnf wh

instance NFData a => NFData (DocInfo a) where
  rnf (DocInfo d s)     = rnf d `seq` rnf s

instance NFData WordInfo where
  rnf (WordInfo t s)    = rnf t `seq` rnf s

instance XmlPickler a => XmlPickler (Result a) where
  xpickle               = xpElem "result" $ 
                          xpWrap ( \ (dh, wh) -> Result dh wh
                                 , \ (Result dh wh) -> (dh, wh)
                                 ) (xpPair xpDocHits xpWordHits)

instance XmlPickler a => XmlPickler (DocInfo a) where
  xpickle               = xpWrap ( \ (d, s) -> DocInfo d s
                                 , \ (DocInfo d s) -> (d, s)
                                 ) xpDocInfo'
    where
    xpDocInfo'          = xpPair xpickle (xpAttr "score" xpPrim)

instance XmlPickler WordInfo where
  xpickle               = xpWrap ( \ (t, s) -> WordInfo t s
                                 , \ (WordInfo t s) -> (t, s)
                                 ) xpWordInfo
    where
    xpWordInfo          = xpPair (xpAttr "term" xpTerms)
                                 (xpAttr "score" xpPrim)
    xpTerms             = xpWrap (split ",", join ",") xpText0

-- ----------------------------------------------------------------------------

-- | The XML pickler for the document hits. Will be sorted by score.
xpDocHits               :: XmlPickler a => PU (DocHits a)
xpDocHits               = xpElem "dochits" $
                          xpWrap ( fromListDocIdMap
                                 , toListSorted
                                 ) (xpList xpDocHit)
  where
  toListSorted          = L.sortBy (compare `on` (docScore . fst . snd)) . toListDocIdMap -- Sort by score
  xpDocHit              = xpElem "doc" $
                          xpPair (xpAttr "idref" xpDocId)
                                 (xpPair xpickle xpDocContextHits)

-- | The XML pickler for the contexts in which the documents were found.
xpDocContextHits        :: PU DocContextHits
xpDocContextHits        = xpWrap (M.fromList, M.toList) $
                          xpList xpDocContextHit
  where
  xpDocContextHit       = xpElem "context" $
                          xpPair (xpAttr "name" xpText) xpDocWordHits

-- | The XML pickler for the words and positions found in a document.
xpDocWordHits           :: PU DocWordHits
xpDocWordHits           = xpWrap (M.fromList, M.toList) (xpList xpDocWordHit)
  where
  xpDocWordHit          = xpElem "word" $
                          xpPair (xpAttr "w" xpText) xpPositions

-- | The XML pickler for the word hits. Will be sorted alphabetically by the words.
xpWordHits              :: PU WordHits
xpWordHits              = xpElem "wordhits" $
                          xpWrap (M.fromList, toListSorted) $
                          xpList xpWordHit
  where
  toListSorted          = L.sortBy (compare `on` fst) . M.toList -- Sort by word
  xpWordHit             = xpElem "word" $
                          xpPair (xpAttr "w" xpText)
                                 (xpPair xpickle xpWordContextHits)

-- | The XML pickler for the contexts in which the words were found.
xpWordContextHits       :: PU WordContextHits
xpWordContextHits       = xpWrap (M.fromList, M.toList) $
                          xpList xpWordContextHit
  where
  xpWordContextHit      = xpElem "context" $
                          xpPair (xpAttr "name" xpText) xpWordDocHits

-- | The XML pickler for the documents and positions where the word occurs (reusing existing pickler).
xpWordDocHits           :: PU WordDocHits
xpWordDocHits           = xpOccurrences

-- ----------------------------------------------------------------------------

-- | Create an empty result.
emptyResult             :: Result a
emptyResult             = Result emptyDocIdMap M.empty

-- | Query the number of documents in a result.
sizeDocHits             :: Result a -> Int
sizeDocHits             = sizeDocIdMap . docHits

-- | Query the number of documents in a result.
sizeWordHits            :: Result a -> Int
sizeWordHits            = M.size . wordHits

-- | Query the maximum score of the documents.
maxScoreDocHits         :: Result a -> Score
maxScoreDocHits         = (foldDocIdMap (\(di, _) r -> max (docScore di) r) 0.0) . docHits

-- | Query the maximum score of the words.
maxScoreWordHits        :: Result a -> Score
maxScoreWordHits        = (M.fold (\(wi, _) r -> max (wordScore wi) r) 0.0) . wordHits

-- | Test if the result contains anything.
null                    :: Result a -> Bool
null                    = nullDocIdMap . docHits

-- | Set the score in a document info.
setDocScore             :: Score -> DocInfo a -> DocInfo a
setDocScore s (DocInfo d _)
                        = DocInfo d s

-- | Set the score in a word info.
setWordScore            :: Score -> WordInfo -> WordInfo
setWordScore s (WordInfo t _)
                        = WordInfo t s

-- | Extract all documents from a result
getDocuments            :: Result a -> [Document a]
getDocuments r          = map (document . fst . snd) $
                          toListDocIdMap (docHits r)

-- ----------------------------------------------------------------------------
