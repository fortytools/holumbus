-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Documents
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  A bijective table between documents and their id's.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Documents 
(
  -- * Documents type
  Documents (..)

  -- * Construction
  , emptyDocuments
)
where

import Text.XML.HXT.Arrow

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Binary

import Control.Parallel.Strategies

import Holumbus.Index.Common

-- | The table which is used to map a document to an artificial id and vice versa.
data Documents     = Documents { idToDoc   :: !(IntMap Document)
                               , docToId   :: !(Map URI DocId) 
                               , lastDocId :: !DocId
                               } deriving (Show, Eq)

instance HolDocuments Documents where
  sizeDocs d = IM.size (idToDoc d)
  
  lookupById = (flip IM.lookup) . idToDoc
  lookupByURI = (flip M.lookup) . docToId

  getText _ _ = ""

  mergeDocs _ _ = emptyDocuments

  insertDoc ds d@(_, u) = maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds u)
    where
    reallyInsert = (newId, Documents newIdToDoc newDocToId newId)
      where
      newIdToDoc = IM.insert newId d (idToDoc ds)
      newDocToId = M.insert u newId (docToId ds)
      newId = (lastDocId ds) + 1

instance NFData Documents where
  rnf (Documents i2d d2i lid) = rnf i2d `seq` rnf d2i `seq` rnf lid

instance XmlPickler Documents where
  xpickle =  xpElem "documents" $ xpWrap convertDoctable (xpWrap (IM.fromList, IM.toList) (xpList xpDocumentWithId))
    where
    convertDoctable = (\itd -> Documents itd (idToDoc2docToId itd) (lastId itd), \(Documents itd _ _) -> itd)
    xpDocumentWithId = xpElem "doc" (xpPair (xpAttr "id" xpPrim) xpDocument)

instance Binary Documents where
  put (Documents i2d _ _) = do
                            put i2d
  get = do
        i2d <- get
        return (Documents i2d (idToDoc2docToId i2d) (lastId i2d))

-- | Create an empty table.
emptyDocuments :: Documents
emptyDocuments = Documents IM.empty M.empty 0

-- | Construct the inverse map from the original map.
idToDoc2docToId :: IntMap Document -> Map URI DocId
idToDoc2docToId = IM.foldWithKey (\i (_, s2) r -> M.insert s2 i r) M.empty

-- | Query the 'idToDoc' part of the document table for the last id.
lastId :: IntMap Document -> Int
lastId = (IM.foldWithKey (\k _ r -> max k r) 0)
