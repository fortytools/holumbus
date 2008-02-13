-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Documents
  Copyright  : Copyright (C) 2007, 2008 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
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
  , fromMap
)
where

import Data.Maybe

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

  mergeDocs d1 d2 = (conflicts, Documents merged (idToDoc2docToId merged) lid)
    where
    (conflicts, merged, lid) = IM.foldWithKey checkDoc ([], (idToDoc d1), (lastDocId d1)) (idToDoc d2)
      where
      checkDoc i (u, t) (c, d, l) = maybe checkId (\ni -> ((i, ni):c, d, l)) (lookupByURI d1 u)
        where
        checkId = if IM.member i d then ((i, l + 1):c, IM.insert (l + 1) (u, t) d, l + 1)
                  else (c, IM.insert i (u, t) d, l)

  insertDoc ds d@(_, u) = maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds u)
    where
    reallyInsert = (newId, Documents newIdToDoc newDocToId newId)
      where
      newIdToDoc = IM.insert newId d (idToDoc ds)
      newDocToId = M.insert u newId (docToId ds)
      newId = (lastDocId ds) + 1

  removeById ds d = maybe ds reallyRemove (lookupById ds d)
    where
    reallyRemove (_, u) = Documents (IM.delete d (idToDoc ds)) (M.delete u (docToId ds)) (lastDocId ds)

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

-- | Create a document table from a single map.
fromMap :: IntMap Document -> Documents
fromMap itd = Documents itd (idToDoc2docToId itd) (lastId itd)

-- | Construct the inverse map from the original map.
idToDoc2docToId :: IntMap Document -> Map URI DocId
idToDoc2docToId = IM.foldWithKey (\i (_, s2) r -> M.insert s2 i r) M.empty

-- | Query the 'idToDoc' part of the document table for the last id.
lastId :: IntMap Document -> Int
lastId = (IM.foldWithKey (\k _ r -> max k r) 0)
