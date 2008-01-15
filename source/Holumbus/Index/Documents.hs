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

import Holumbus.Control.Sequence

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

instance DeepSeq Documents where
  deepSeq (Documents i2d d2i lid) b = deepSeq i2d $ deepSeq d2i $ deepSeq lid b

instance XmlPickler Documents where
  xpickle =  xpWrap convertDoctable (xpWrap (IM.fromList, IM.toList) (xpList xpDocumentWithId))
    where
    convertDoctable = (\itd -> Documents itd (itd2dti itd) (lastId itd), \(Documents itd _ _) -> itd)
    itd2dti = IM.foldWithKey (\i (_, s2) r -> M.insert s2 i r) M.empty
    xpDocumentWithId = xpElem "doc" (xpPair (xpAttr "id" xpPrim) xpDocument)

-- | The XML pickler for a single document.
xpDocument :: PU Document
xpDocument = xpPair xpTitle xpURI
  where
  xpURI           = xpAttr "href" xpText
  xpTitle         = xpAttr "title" xpText
  
-- | The XML pickler for the document table.
xpDocuments :: PU Documents
xpDocuments = xpElem "documents" $ xpickle

-- | Create an empty table.
emptyDocuments :: Documents
emptyDocuments = Documents IM.empty M.empty 0

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
insertWithId :: Document -> Documents -> (DocId, Documents)
insertWithId d@(_, u) ds = (newId, Documents newIdToDoc newDocToId newId)
  where
  newIdToDoc = IM.insert newId d (idToDoc ds)
  newDocToId = M.insert u newId (docToId ds)
  newId = (lastDocId ds) + 1

-- | Insert a document into the table.
insert :: Document -> Documents -> Documents
insert = (snd . ) . insertWithId
