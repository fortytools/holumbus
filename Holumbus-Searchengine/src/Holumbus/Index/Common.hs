{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common
  Copyright  : Copyright (C) 2007-2012 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  Common data types shared by all index types and a unified interface for
  all different index types. This module defines the common interfaces of
  indexes and their document tables as well as full-text caches.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common 
  (
  -- * Common index types and classes
  HolIndex (..)
  , HolIndexM (..)
  , HolDocuments (..)
  , HolDocIndex (..)
  , HolCache (..)

  -- * Indexes and Documents
  -- , mergeAll

  , module Holumbus.Index.Common.BasicTypes
  , module Holumbus.Index.Common.Document
  , module Holumbus.Index.Common.DocId
  , module Holumbus.Index.Common.DocIdMap
  , module Holumbus.Index.Common.Occurences
  , module Holumbus.Index.Common.RawResult
  , module Holumbus.Index.Common.LoadStore

  )
where

import Control.Monad                    ( foldM )

-- import Data.Binary                   ( Binary (..) )
-- import Data.Maybe

import Holumbus.Index.Common.BasicTypes
import Holumbus.Index.Common.Document
import Holumbus.Index.Common.DocId
import Holumbus.Index.Common.DocIdMap
import Holumbus.Index.Common.Occurences
import Holumbus.Index.Common.RawResult
import Holumbus.Index.Common.LoadStore

-- ------------------------------------------------------------

-- | This class provides a generic interface to different types of index implementations.

class HolIndex i where
  -- | Returns the number of unique words in the index.
  sizeWords                     :: i -> Int

  -- | Returns a list of all contexts avaliable in the index.
  contexts                      :: i -> [Context]

  -- | Returns the occurrences for every word. A potentially expensive operation.
  allWords                      :: i -> Context -> RawResult

  -- | Searches for words beginning with the prefix in a given context (case-sensitive).
  prefixCase                    :: i -> Context -> String -> RawResult

  -- | Searches for words beginning with the prefix in a given context (case-insensitive).
  prefixNoCase                  :: i -> Context -> String -> RawResult

  -- | Searches for and exact word in a given context (case-sensitive).
  lookupCase                    :: i -> Context -> String -> RawResult

  -- | Searches for and exact word in a given context (case-insensitive).
  lookupNoCase                  :: i -> Context -> String -> RawResult
  
  -- | Insert occurrences.
  insertOccurrences             :: Context -> Word -> Occurrences -> i -> i

  -- | Delete occurrences.
  deleteOccurrences             :: Context -> Word -> Occurrences -> i -> i

  -- | Insert a position for a single document.
  insertPosition                :: Context -> Word -> DocId -> Position -> i -> i
  insertPosition c w d p i      = insertOccurrences c w (singletonOccurrence d p) i

  -- | Delete a position for a single document.
  deletePosition                :: Context -> Word -> DocId -> Position -> i -> i
  deletePosition c w d p i      = deleteOccurrences c w (singletonOccurrence d p) i

  -- | Merges two indexes. 
  mergeIndexes                  :: i -> i -> i

  -- | Substract one index from another.
  substractIndexes              :: i -> i -> i

  -- | Splitting an index by its contexts.
  splitByContexts               :: i -> Int -> [i]

  -- | Splitting an index by its documents.
  splitByDocuments              :: i -> Int -> [i]

  -- | Splitting an index by its words.
  splitByWords                  :: i -> Int -> [i]

  -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
  -- to the same new id, the two sets of word positions will be merged if both old id's are present
  -- in the occurrences for a word in a specific context.
  updateDocIds                  :: (Context -> Word -> DocId -> DocId) -> i -> i

  -- | Update document id's with a simple injective editing function.
  updateDocIds'                 :: (DocId -> DocId) -> i -> i
  updateDocIds' f               = updateDocIds (const . const $ f)

  -- Convert an Index to a list. Can be used for easy conversion between different index  
  -- implementations

  toList                        :: i -> [(Context, Word, Occurrences)]
  
  -- Create an Index from a list. Can be used for easy conversion between different index  
  -- implementations. Needs an empty index as first argument

  fromList                      :: i -> [(Context, Word, Occurrences)] -> i
  fromList e                    = foldl (\i (c,w,o) -> insertOccurrences c w o i) e

-- ------------------------------------------------------------

-- | This class provides a generic interface to different monadic types of index implementations.

class (Monad m) => HolIndexM m i where
  -- | Returns the number of unique words in the index.
  sizeWordsM                    :: i -> m Int

  -- | Returns a list of all contexts avaliable in the index.
  contextsM                     :: i -> m [Context]

  -- | Returns the occurrences for every word. A potentially expensive operation.
  allWordsM                     :: i -> Context -> m RawResult

  -- | Searches for words beginning with the prefix in a given context (case-sensitive).
  prefixCaseM                   :: i -> Context -> String -> m RawResult

  -- | Searches for words beginning with the prefix in a given context (case-insensitive).
  prefixNoCaseM                 :: i -> Context -> String -> m RawResult

  -- | Searches for and exact word in a given context (case-sensitive).
  lookupCaseM                   :: i -> Context -> String -> m RawResult

  -- | Searches for and exact word in a given context (case-insensitive).
  lookupNoCaseM                 :: i -> Context -> String -> m RawResult

  -- | Insert occurrences.
  insertOccurrencesM            :: Context -> Word -> Occurrences -> i -> m i

  -- | Delete occurrences.
  deleteOccurrencesM            :: Context -> Word -> Occurrences -> i -> m i

  -- | Insert a position for a single document.
  insertPositionM               :: Context -> Word -> DocId -> Position -> i -> m i
  insertPositionM c w d p i     = insertOccurrencesM c w (singletonOccurrence d p) i

  -- | Delete a position for a single document.
  deletePositionM               :: Context -> Word -> DocId -> Position -> i -> m i
  deletePositionM c w d p i     = deleteOccurrencesM c w (singletonOccurrence d p) i

  -- | Merges two indexes. 
  mergeIndexesM                 :: i -> i -> m i

  -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
  -- to the same new id, the two sets of word positions will be merged if both old id's are present
  -- in the occurrences for a word in a specific context.
  updateDocIdsM                 :: (Context -> Word -> DocId -> DocId) -> i -> m i

  -- | Update document id's with an simple injective editing function.
  updateDocIdsM'                :: (DocId -> DocId) -> i -> m i

  -- Convert an Index to a list. Can be used for easy conversion between different index  
  -- implementations
  toListM                       :: i -> m [(Context, Word, Occurrences)]

  -- Create an Index from a list. Can be used vor easy conversion between different index  
  -- implementations. Needs an empty index as first argument
  fromListM                     :: i -> [(Context, Word, Occurrences)] -> m i
  fromListM e                   = foldM (\i (c,w,o) -> insertOccurrencesM c w o i) e

-- ------------------------------------------------------------

-- don't change IO into Monad m
-- this leads to ambiguities and error messages, when a context (HolIndexM m i) is used
--
-- NOT: instance (Monad m, HolIndex i) => HolIndexM m i where

instance (HolIndex i) => HolIndexM IO i where
    sizeWordsM                  = return . sizeWords
    contextsM                   = return . contexts
    allWordsM i                 = return . allWords i
    prefixCaseM i c             = return . prefixCase i c
    prefixNoCaseM i c           = return . prefixNoCase i c
    lookupCaseM i c             = return . lookupCase i c
    lookupNoCaseM i c           = return . lookupNoCase i c
    insertOccurrencesM c w o    = return . insertOccurrences c w o
    deleteOccurrencesM c w o    = return . deleteOccurrences c w o
    mergeIndexesM i1            = return . mergeIndexes i1
    updateDocIdsM u             = return . updateDocIds u
    updateDocIdsM' f            = return . updateDocIds (const . const $ f)
    toListM                     = return . toList

-- ------------------------------------------------------------

class HolDocuments d a where
  -- | doctable empty?
  nullDocs                      :: d a -> Bool
  nullDocs                      = (== 0) . sizeDocs

  -- | Returns the number of unique documents in the table.
  sizeDocs                      :: d a -> Int
  
  -- | Lookup a document by its id.
  lookupById                    :: Monad m => d a -> DocId -> m (Document a)

  -- | Lookup the id of a document by an URI.
  lookupByURI                   :: Monad m => d a -> URI -> m DocId

  -- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
  -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
  -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

  unionDocs                     :: d a -> d a -> d a
  unionDocs dt1                 = foldDocIdMap addDoc dt1 . toMap
      where
      addDoc d dt               = snd . insertDoc dt $ d

  -- | Test whether the doc ids of both tables are disjoint
  disjointDocs                  :: d a -> d a -> Bool

  -- | Return an empty document table. The input parameter is taken to identify the typeclass
  makeEmpty                     :: d a -> d a
  
  -- | Insert a document into the table. Returns a tuple of the id for that document and the 
  -- new table. If a document with the same URI is already present, its id will be returned 
  -- and the table is returned unchanged.

  insertDoc                     :: d a -> (Document a) -> (DocId, d a)

  -- | Update a document with a certain DocId. 
  updateDoc                     :: d a -> DocId -> (Document a) -> d a

  -- | Removes the document with the specified id from the table.
  removeById                    :: d a -> DocId -> d a

  -- | Removes the document with the specified URI from the table.
  removeByURI                   :: d a -> URI -> d a
  removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)

  -- | Update documents (through mapping over all documents).
  updateDocuments               :: (Document a -> Document a) -> d a -> d a

  filterDocuments               :: (Document a -> Bool) -> d a -> d a

  -- | Create a document table from a single map.
  fromMap                       :: DocIdMap (Document a) -> d a

  -- | Convert document table to a single map
  toMap                         :: d a -> DocIdMap (Document a)

  -- | Edit document ids
  editDocIds                    :: (DocId -> DocId) -> d a -> d a
  editDocIds f                  = fromMap . foldWithKeyDocIdMap (insertDocIdMap . f) emptyDocIdMap . toMap

-- ------------------------------------------------------------

class HolCache c where
  -- | Retrieves the full text of a document for a given context. Will never throw any exception,
  -- upon failure or if no text found for the document, @Nothing@ is returned.
  getDocText  :: c -> Context -> DocId -> IO (Maybe Content)

  -- | Store the full text of a document for a given context. May throw an exception if the 
  -- storage of the text failed.

  putDocText  :: c -> Context -> DocId -> Content -> IO ()
  -- | Merge two caches in the way that everything that is in the second cache is inserted into the
  --   first one.

  mergeCaches :: c -> c -> IO c

-- ------------------------------------------------------------

class (HolDocuments d a, HolIndex i) => HolDocIndex d a i where

    -- | Merge two doctables and indexes together into a single doctable and index
    unionDocIndex               :: d a -> i -> d a -> i -> (d a, i)

    -- | Defragment a doctable and index, useful when the doc ids are
    -- organized as an intervall of ints.
    --
    -- Default implementation is the identity

    defragmentDocIndex          :: d a -> i -> (d a, i)
    defragmentDocIndex          = (,)

-- ------------------------------------------------------------
