-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Documents
  Copyright  : Copyright (C) 2007, 2008 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  A bijective table between documents and their id's. Implemented on top of
  "Data.Map" and "Data.IntMap".
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}

module Holumbus.Index.Documents 
(
  -- * Documents type
  Documents (..)

  -- * Construction
  , emptyDocuments
  , singleton
  , fromMap
  , toMap
  
  -- * Conversion
  , simplify
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
data Documents a = Documents
  { idToDoc   :: !(IntMap (Document a)) -- ^ A mapping from a document id to the document itself.
  , docToId   :: !(Map URI DocId)       -- ^ A mapping from the URI of a document to its id.
  , lastDocId :: !DocId                 -- ^ The last used document id.
  }
  deriving (Show)

instance Binary a => HolDocuments Documents a where
  sizeDocs d = IM.size (idToDoc d)
  
  lookupById = (flip IM.lookup) . idToDoc
  lookupByURI = (flip M.lookup) . docToId

  mergeDocs d1 d2 = (conflicts, Documents merged (idToDoc2docToId merged) lid)
    where
    (conflicts, merged, lid) = IM.foldWithKey checkDoc ([], (idToDoc d1), (lastDocId d1)) (idToDoc d2)
      where
      checkDoc i doc (c, d, l) = maybe checkId (\ni -> ((i, ni):c, d, l)) (lookupByURI d1 (uri doc))
        where
        checkId = if IM.member i d then let ni = l + 1 in ((i, ni):c, IM.insert ni doc d, ni)
                  else (c, IM.insert i doc d, max i l)

  makeEmpty _ = emptyDocuments

  insertDoc ds d = maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds (uri d))
    where
    reallyInsert = (newId, Documents newIdToDoc newDocToId newId)
      where
      newIdToDoc = IM.insert newId d (idToDoc ds)
      newDocToId = M.insert (uri d) newId (docToId ds)
      newId = (lastDocId ds) + 1

  updateDoc ds i d = ds 
                     { idToDoc = IM.insert i d (idToDoc ds)
                     , docToId = M.insert (uri d) i (docToId (removeById ds i))
                     }


  removeById ds d = maybe ds reallyRemove (lookupById ds d)
    where
    reallyRemove (Document _ u _) = Documents (IM.delete d (idToDoc ds)) (M.delete u (docToId ds)) (lastDocId ds)

  updateDocuments f d = Documents updated (idToDoc2docToId updated) (lastId updated)
    where
    updated = IM.map f (idToDoc d)

  filterDocuments p d = Documents filtered (idToDoc2docToId filtered) (lastId filtered)
    where 
    filtered = IM.filter p (idToDoc d)  

  fromMap itd = Documents itd (idToDoc2docToId itd) (lastId itd)

  toMap = idToDoc

-- Ignoring last document id when testing for equality
instance Eq a => Eq (Documents a) where
  (==) (Documents i2da d2ia _) (Documents i2db d2ib _) = (i2da == i2db) && (d2ia == d2ib)

instance NFData a => NFData (Documents a) where
  rnf (Documents i2d d2i lid) = rnf i2d `seq` rnf d2i `seq` rnf lid

instance XmlPickler a => XmlPickler (Documents a) where
  xpickle =  xpElem "documents" $ xpWrap convertDoctable (xpWrap (IM.fromList, IM.toList) (xpList xpDocumentWithId))
    where
    convertDoctable = (\itd -> Documents itd (idToDoc2docToId itd) (lastId itd), \(Documents itd _ _) -> itd)
    xpDocumentWithId = xpElem "doc" (xpPair (xpAttr "id" xpPrim) xpickle)

instance Binary a => Binary (Documents a) where
  put (Documents i2d _ _) = put i2d
  get = do
        i2d <- get
        return (Documents i2d (idToDoc2docToId i2d) (lastId i2d))

-- | Create an empty table.
emptyDocuments :: Documents a
emptyDocuments = Documents IM.empty M.empty 0

-- | Create a document table containing a single document.
singleton :: Document a -> Documents a
singleton d = Documents (IM.singleton 1 d) (M.singleton (uri d) 1) 1

-- | Simplify a document table by transforming the custom information into a string.
simplify :: Show a => Documents a -> Documents String
simplify dt = Documents (simple (idToDoc dt)) (docToId dt) (lastDocId dt)
  where
  simple i2d = IM.map (\d -> Document (title d) (uri d) (maybe Nothing (Just . show) (custom d))) i2d

-- | Construct the inverse map from the original map.
idToDoc2docToId :: IntMap (Document a) -> Map URI DocId
idToDoc2docToId = IM.foldWithKey (\i d r -> M.insert (uri d) i r) M.empty

-- | Query the 'idToDoc' part of the document table for the last id.
lastId :: IntMap (Document a) -> Int
lastId = (IM.foldWithKey (\k _ r -> max k r) 0)
