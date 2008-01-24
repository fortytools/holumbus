-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Documents
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A bijective table between documents and their id's.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Documents 
(
  -- * Documents type
  Documents (..)
  , Document
  , DocId
  , URI
  , Title

  -- * Construction
  , emptyDocuments
  , insertWithId
  , insert

  -- * Query
  , size
  , lookupId
  , lookupURI
  , lastId

  -- * Pickling
  , xpDocuments
  , xpDocument
)
where

import Text.XML.HXT.Arrow.Pickle

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Word
import Data.Binary

import Control.Parallel.Strategies

-- | The table which is used to map a document to an artificial id and vice versa.
data Documents     = Documents { idToDoc   :: !(IntMap Document)
                               , docToId   :: !(Map URI DocId) 
                               , lastDocId :: !DocId
                               } deriving (Show, Eq)

-- | A document consists of a title and it's unique identifier.
type Document      = (Title, URI)

-- | The unique identifier of a document (created upon insertion into the document table).
type DocId         = Int
-- | The URI describing the location of the original document.
type URI           = String
-- | The title of a document.
type Title         = String

instance NFData Documents where
  rnf (Documents i2d d2i lid) = rnf i2d `seq` rnf d2i `seq` rnf lid

instance XmlPickler Documents where
  xpickle =  xpWrap convertDoctable (xpWrap (IM.fromList, IM.toList) (xpList xpDocumentWithId))
    where
    convertDoctable = (\itd -> Documents itd (idToDoc2docToId itd) (lastId itd), \(Documents itd _ _) -> itd)
    xpDocumentWithId = xpElem "doc" (xpPair (xpAttr "id" xpPrim) xpDocument)

instance Binary Documents where
  put (Documents i2d _ _) = do
                            put i2d
  get = do
        i2d <- get
        return (Documents i2d (idToDoc2docToId i2d) (lastId i2d))

-- | The XML pickler for a single document.
xpDocument :: PU Document
xpDocument = xpPair xpTitle xpURI
  where
  xpURI           = xpAttr "href" xpText
  xpTitle         = xpAttr "title" xpText
  
-- | The XML pickler for the document table.
xpDocuments :: PU Documents
xpDocuments = xpElem "documents" $ xpickle

-- | Construct the inverse map from the original map.
idToDoc2docToId :: IntMap Document -> Map URI DocId
idToDoc2docToId = IM.foldWithKey (\i (_, s2) r -> M.insert s2 i r) M.empty

-- | Create an empty table.
emptyDocuments :: Documents
emptyDocuments = Documents IM.empty M.empty 0

-- | Returns the number of documents in the table.
size :: Documents -> Int
size d = IM.size (idToDoc d)

-- | Query the @idToDoc@ part of the document table for the last id.
lastId :: IntMap Document -> Int
lastId = (IM.foldWithKey (\k _ r -> max k r) 0)

-- | Look for an URI in the document table.
lookupURI :: URI -> Documents -> Maybe DocId
lookupURI = (. docToId) . M.lookup

-- | Look for an document id in the document table.
lookupId :: DocId -> Documents -> Maybe Document
lookupId = (. idToDoc) . IM.lookup

-- | Insert a document into the table. Returns a tuple of the id for that document and the new table.
-- If a document with the same URI is already present, its id will be returned with unchanged @Documents@.
insertWithId :: Document -> Documents -> (DocId, Documents)
insertWithId d@(_, u) ds = maybe reallyInsert (\oldId -> (oldId, ds)) (lookupURI u ds)
  where
  reallyInsert = (newId, Documents newIdToDoc newDocToId newId)
    where
    newIdToDoc = IM.insert newId d (idToDoc ds)
    newDocToId = M.insert u newId (docToId ds)
    newId = (lastDocId ds) + 1

-- | Insert a document into the table.
insert :: Document -> Documents -> Documents
insert = (snd . ) . insertWithId
