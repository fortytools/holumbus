-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common
  Copyright  : Copyright (C) 2007, 2008 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  Common data types shared by all index types and a unified interface for
  all different index types. This module defines the common interfaces of
  indexes and their document tables as well as full-text caches.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}

module Holumbus.Index.Common 
  (
  -- * Common index types and classes
  Position
  , Context
  , Document (..)
  , DocId
  , URI
  , Title
  , Content
  , Word
  , Occurrences
  , Positions
  , RawResult
  , HolIndex (..)
  , HolIndexM (..)
  , HolDocuments (..)
  , HolCache (..)

  -- * Indexes and Documents
  , mergeAll
  , resultByDocument
  , resultByWord

  -- * Occurrences
  , emptyOccurrences
  , sizeOccurrences
  , mergeOccurrences
  , substractOccurrences

  -- * Pickling
  , xpOccurrences
  , xpPositions
  
  -- * Persistent storage
  , loadFromFile

  , loadFromXmlFile
  , loadFromBinFile
  , writeToXmlFile
  , writeToBinFile
  )
where

import Text.XML.HXT.Arrow

import Data.Maybe

import qualified Data.List as L

import Data.Binary (Binary (..))
import qualified Data.Binary as B

import Control.Monad (liftM3, foldM)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Control.Parallel.Strategies

import Holumbus.Control.MapReduce.MapReducible

-- | A document consists of a title and its unique identifier.
data Document a = Document
  { title  :: !Title
  , uri    :: !URI
  , custom :: !(Maybe a)
  }
  deriving (Show, Eq, Ord)

instance Binary a => Binary (Document a) where
  put (Document t u c) = put t >> put u >> put c
  get = liftM3 Document get get get

instance XmlPickler a => XmlPickler (Document a) where
  xpickle = xpWrap (\(t, u, i) -> Document t u i, \(Document t u i) -> (t, u, i)) (xpTriple xpTitle xpURI xpickle)
    where
    xpURI           = xpAttr "href" xpText0
    xpTitle         = xpAttr "title" xpText0

instance NFData a => NFData (Document a) where
  rnf (Document t u c) = rnf t `seq` rnf u `seq` rnf c

-- | The unique identifier of a document (created upon insertion into the document table).
type DocId         = Int
-- | The URI describing the location of the original document.
type URI           = String
-- | The title of a document.
type Title         = String
-- | The content of a document.
type Content       = String

-- | The position of a word in the document.
type Position      = Int
-- | The name of a context.
type Context       = String
-- | A single word.
type Word          = String

-- | The occurrences in a number of documents. A mapping from document ids to the positions in the document.
type Occurrences   = IntMap Positions
-- | The positions of the word in the document.
type Positions     = IntSet

-- | The raw result returned when searching the index.
type RawResult     = [(Word, Occurrences)]

class (Monad m) => HolIndexM m i where
  -- | Returns the number of unique words in the index.
  sizeWordsM    :: i -> m Int
  -- | Returns a list of all contexts avaliable in the index.
  contextsM     :: i -> m [Context]

  -- | Returns the occurrences for every word. A potentially expensive operation.
  allWordsM     :: i -> Context -> m RawResult
  -- | Searches for words beginning with the prefix in a given context (case-sensitive).
  prefixCaseM   :: i -> Context -> String -> m RawResult
  -- | Searches for words beginning with the prefix in a given context (case-insensitive).
  prefixNoCaseM :: i -> Context -> String -> m RawResult
  -- | Searches for and exact word in a given context (case-sensitive).
  lookupCaseM   :: i -> Context -> String -> m RawResult
  -- | Searches for and exact word in a given context (case-insensitive).
  lookupNoCaseM :: i -> Context -> String -> m RawResult
  
    -- | Insert occurrences.
  insertOccurrencesM :: Context -> Word -> Occurrences -> i -> m i
  -- | Delete occurrences.
  deleteOccurrencesM :: Context -> Word -> Occurrences -> i -> m i
  
  -- | Insert a position for a single document.
  insertPositionM :: Context -> Word -> DocId -> Position -> i -> m i
  insertPositionM c w d p i = insertOccurrencesM c w (IM.singleton d (IS.singleton p)) i
  -- | Delete a position for a single document.
  deletePositionM :: Context -> Word -> DocId -> Position -> i -> m i
  deletePositionM c w d p i = deleteOccurrencesM c w (IM.singleton d (IS.singleton p)) i

  -- | Merges two indexes. 
  mergeIndexesM  :: i -> i -> m i

  -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
  -- to the same new id, the two sets of word positions will be merged if both old id's are present
  -- in the occurrences for a word in a specific context.
  updateDocIdsM:: (Context -> Word -> DocId -> DocId) -> i -> m i

  -- Convert an Index to a list. Can be used for easy conversion between different index  
  -- implementations
  toListM   :: i -> m [(Context, Word, Occurrences)]
  
  -- Create an Index from a a list. Can be used vor easy conversion between different index  
  -- implementations. Needs an empty index as first argument
  fromListM :: i -> [(Context, Word, Occurrences)] -> m i
  fromListM e = foldM (\i (c,w,o) -> insertOccurrencesM c w o i) e

-- | This class provides a generic interface to different types of index implementations.
class (Binary i, MapReducible i Context (Word, DocId, Position) ) => HolIndex i where
  -- | Returns the number of unique words in the index.
  sizeWords     :: i -> Int
  -- | Returns a list of all contexts avaliable in the index.
  contexts      :: i -> [Context]

  -- | Returns the occurrences for every word. A potentially expensive operation.
  allWords      :: i -> Context -> RawResult
  -- | Searches for words beginning with the prefix in a given context (case-sensitive).
  prefixCase    :: i -> Context -> String -> RawResult
  -- | Searches for words beginning with the prefix in a given context (case-insensitive).
  prefixNoCase  :: i -> Context -> String -> RawResult
  -- | Searches for and exact word in a given context (case-sensitive).
  lookupCase    :: i -> Context -> String -> RawResult
  -- | Searches for and exact word in a given context (case-insensitive).
  lookupNoCase  :: i -> Context -> String -> RawResult
  
  -- | Insert occurrences.
  insertOccurrences :: Context -> Word -> Occurrences -> i -> i
  -- | Delete occurrences.
  deleteOccurrences :: Context -> Word -> Occurrences -> i -> i

  -- | Insert a position for a single document.
  insertPosition :: Context -> Word -> DocId -> Position -> i -> i
  insertPosition c w d p i = insertOccurrences c w (IM.singleton d (IS.singleton p)) i
  -- | Delete a position for a single document.
  deletePosition :: Context -> Word -> DocId -> Position -> i -> i
  deletePosition c w d p i = deleteOccurrences c w (IM.singleton d (IS.singleton p)) i

  -- | Merges two indexes. 
  mergeIndexes  :: i -> i -> i
  -- | Substract one index from another.
  substractIndexes :: i -> i -> i

  -- | Splitting an index by its contexts.
  splitByContexts :: i -> Int -> [i]
  -- | Splitting an index by its documents.
  splitByDocuments :: i -> Int -> [i]
  -- | Splitting an index by its words.
  splitByWords :: i -> Int -> [i]

  -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
  -- to the same new id, the two sets of word positions will be merged if both old id's are present
  -- in the occurrences for a word in a specific context.
  updateDocIds:: (Context -> Word -> DocId -> DocId) -> i -> i
  
  -- Convert an Index to a list. Can be used for easy conversion between different index  
  -- implementations
  toList   :: i -> [(Context, Word, Occurrences)]
  
  -- Create an Index froma a list. Can be used vor easy conversion between different index  
  -- implementations. Needs an empty index as first argument
  fromList :: i -> [(Context, Word, Occurrences)] -> i
  fromList e = foldl (\i (c,w,o) -> insertOccurrences c w o i) e



class Binary (d a) => HolDocuments d a where
  -- | Returns the number of unique documents in the table.
  sizeDocs      :: d a -> Int
  
  -- | Lookup a document by its id.
  lookupById    :: Monad m => d a -> DocId -> m (Document a)
  -- | Lookup the id of a document by an URI.
  lookupByURI   :: Monad m => d a -> URI -> m DocId
  
  -- | Merge two document tables. The returned tuple contains a list of id's from the second
  -- table that were replaced with new id's to avoid collisions.
  mergeDocs     :: d a -> d a -> ([(DocId, DocId)], d a)

  -- | Insert a document into the table. Returns a tuple of the id for that document and the 
  -- new table. If a document with the same URI is already present, its id will be returned 
  -- and the table is returned unchanged.
  insertDoc     :: d a -> (Document a) -> (DocId, d a)

  -- | Update a document with a certain DocId. 
  updateDoc     :: d a -> DocId -> (Document a) -> d a

  -- | Removes the document with the specified id from the table.
  removeById     :: d a -> DocId -> d a
  -- | Removes the document with the specified URI from the table.
  removeByURI    :: d a -> URI -> d a
  removeByURI ds u = maybe ds (removeById ds) (lookupByURI ds u)

  -- | Update documents (through mapping over all documents).
  updateDocuments :: (Document a -> Document b) -> d a -> d b

  filterDocuments :: (Document a -> Bool) -> d a -> d a

  -- | Create a document table from a single map.
  fromMap :: IntMap (Document a) -> d a

  -- | Convert document table to a single map
  toMap :: d a -> IntMap (Document a)

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

-- | The XML pickler for a set of positions.
xpPositions :: PU Positions
xpPositions = xpWrap ( IS.fromList . (map read) . words
                     , unwords . (map show) . IS.toList
                     ) xpText

-- | The XML pickler for the occurrences of a word.
xpOccurrences :: PU Occurrences
xpOccurrences = xpWrap (IM.fromList, IM.toList) (xpList xpOccurrence)
  where
  xpOccurrence = xpElem "doc" (xpPair (xpAttr "idref" xpPrim) xpPositions)

-- | Merges an index with its documents table with another index and its documents table. 
-- Conflicting id's for documents will be resolved automatically.
mergeAll :: (HolDocuments d a, HolIndex i) => d a -> i -> d a -> i -> (d a, i)
mergeAll d1 i1 d2 i2 = (md, mergeIndexes i1 (updateDocIds replaceIds i2))
  where
  (ud, md) = mergeDocs d1 d2
  idTable = IM.fromList ud
  replaceIds _ _ d = fromMaybe d (IM.lookup d idTable)

-- | Create an empty set of positions.
emptyOccurrences :: Occurrences
emptyOccurrences = IM.empty

-- | Determine the number of positions in a set of occurrences.
sizeOccurrences :: Occurrences -> Int
sizeOccurrences = IM.fold ((+) . IS.size) 0

-- | Merge two occurrences.
mergeOccurrences :: Occurrences -> Occurrences -> Occurrences
mergeOccurrences = IM.unionWith IS.union

-- | Substract occurrences from some other occurrences.
substractOccurrences :: Occurrences -> Occurrences -> Occurrences
substractOccurrences = IM.differenceWith substractPositions
  where
  substractPositions p1 p2 = if IS.null diffPos then Nothing else Just diffPos
    where
    diffPos = IS.difference p1 p2

-- | Transform the raw result into a tree structure ordered by word.
resultByWord :: Context -> RawResult -> Map Word (Map Context Occurrences)
resultByWord c = M.fromList . map (\(w, o) -> (w, M.singleton c o))

-- | Transform the raw result into a tree structure ordered by document.
resultByDocument :: Context -> RawResult -> IntMap (Map Context (Map Word Positions))
resultByDocument c os = IM.map transform $ IM.unionsWith (flip $ (:) . head) (map insertWords os)
  where
  insertWords (w, o) = IM.map (\p -> [(w, p)]) o   
  transform w = M.singleton c (M.fromList w)

-- | Try to determine the file type automatically. The file is loaded as XML if the filename
-- ends with \".xml\" and otherwise is loaded as binary file.
loadFromFile :: (XmlPickler a, Binary a) => FilePath -> IO a
loadFromFile f = if L.isSuffixOf ".xml" f then loadFromXmlFile f else loadFromBinFile f
 
-- | Load from XML file.
loadFromXmlFile :: XmlPickler a => FilePath -> IO a
loadFromXmlFile f = do
                    r <- runX (xunpickleDocument xpickle options f)
                    return $! head r
                    where
                    options = [ (a_remove_whitespace, v_1), (a_encoding, utf8), (a_validate, v_0) ]     

-- | Write to XML file.
writeToXmlFile :: XmlPickler a => FilePath -> a -> IO ()
writeToXmlFile f i = do
                     runX (constA i >>> xpickleDocument xpickle options f)
                     return ()
                     where
                     options = [ (a_indent, v_1), (a_output_encoding, utf8), (a_validate, v_0) ]     

-- | Load from a binary file.
loadFromBinFile :: Binary a => FilePath -> IO a
loadFromBinFile = B.decodeFile

-- | Write to a binary file.
writeToBinFile :: Binary a => FilePath -> a -> IO ()
writeToBinFile = B.encodeFile
