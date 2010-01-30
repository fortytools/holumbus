-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.SmallDocuments
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A bijective table between documents and their id's. Implemented on top of
  "Data.IntMap". Only the lookup by id is fast, while the lookup by URI will
  be very slow, but the memory consumption is much better than with
  "Holumbus.Index.Documents".
-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.SmallDocuments 
(
  -- * Documents type
  SmallDocuments (..)

  -- * Construction
  , emptyDocuments
  , singleton
)
where

-- import Data.Maybe

import Control.Monad

import Text.XML.HXT.Arrow

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Binary

import Holumbus.Index.Common

type SmallDocument a = (ByteString, ByteString, Maybe a)

-- | The table which is used to map a document to an artificial id and vice versa.
data SmallDocuments a = SmallDocuments
  { idToDoc   :: !(IntMap (SmallDocument a)) -- ^ A mapping from a document id to the document itself.
  }
  deriving (Show)

instance Binary a => HolDocuments SmallDocuments a where
  sizeDocs d = IM.size (idToDoc d)

  lookupById ds k = (liftM toDocument) $ maybe (fail "") return $ IM.lookup k $ idToDoc ds

  lookupByURI = error "Not yet implemented"

  mergeDocs _ _ = error "Not yet implemented"

  makeEmpty _ = emptyDocuments

  insertDoc _ _ = error "Not yet implemented"

  updateDoc _ _ _ = error "Not yet implemented"

  removeById _ _ = error "Not yet implemented"

  updateDocuments _ _ = error "Not yet implemented"

  filterDocuments _ _ = error "Not yet implemented"

  fromMap = SmallDocuments . (IM.map fromDocument)

  toMap = (IM.map toDocument) . idToDoc

instance Binary a => Binary (SmallDocuments a) where
  put (SmallDocuments i2d) = put (IM.toAscList i2d)
  get = do
        !i2d <- get
        return $! SmallDocuments $! i2d

instance XmlPickler a => XmlPickler (SmallDocuments a) where
  xpickle =  xpElem "documents" $ xpWrap convertDoctable (xpWrap (IM.fromList, IM.toList) (xpList xpDocumentWithId))
    where
    convertDoctable = (SmallDocuments . (IM.map fromDocument), (IM.map toDocument) . idToDoc)
    xpDocumentWithId = xpElem "doc" (xpPair (xpAttr "id" xpPrim) xpickle)

-- | Create an empty table.
emptyDocuments :: SmallDocuments a
emptyDocuments = SmallDocuments IM.empty

-- | Create a document table containing a single document.
singleton :: Document a -> SmallDocuments a
singleton d = SmallDocuments (IM.singleton 1 (fromDocument d))

-- | Convert a SmallDocument to a Document.
toDocument :: SmallDocument a -> Document a
toDocument (t, u, v) = Document (B.toString t) (B.toString u) v

-- | Convert a Document to a SmallDocument.
fromDocument :: Document a -> SmallDocument a
fromDocument (Document t u v) = (B.fromString t, B.fromString u, v)
