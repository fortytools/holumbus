{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.HashedDocuments
  Copyright  : Copyright (C) 2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental

  A more space efficient substitute for Holumbus.Index.Documents
  and a more flexible implementation than Holumbus.Index.CompactDocuments.

  DocIds are computed by a hash function, so the inverse map from URIs to DocIds
  is substituted by the hash function
-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.HashedDocuments 
    (
      -- * Documents type
      Documents (..)
    , CompressedDoc(..)
    , DocMap

      -- * Construction
    , emptyDocuments
    , singleton
  
      -- * Conversion
    , toDocument
    , fromDocument
    , fromDocMap
    , toDocMap
    )
where

import qualified Codec.Compression.BZip as BZ

import           Control.DeepSeq

import           Data.Binary            ( Binary )
import qualified Data.Binary            as B

import           Data.ByteString.Lazy   ( ByteString )
import qualified Data.ByteString.Lazy   as BS

import           Data.Digest.Pure.SHA

import           Holumbus.Index.Common

import           Text.XML.HXT.Core

-- ----------------------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.

type DocMap a
    = DocIdMap (CompressedDoc a)

newtype CompressedDoc a
    = CDoc { unCDoc :: ByteString }
      deriving (Eq, Show)

newtype Documents a
    = Documents { idToDoc   :: DocMap a }   -- ^ A mapping from a document id to
                                            --   the document itself.
      deriving (Eq, Show, NFData)

-- ----------------------------------------------------------------------------
-- | The hash function from URIs to DocIds

docToId :: URI -> DocId
docToId = mkDocId . integerDigest . sha1 . B.encode

-- ----------------------------------------------------------------------------

instance (Binary a, HolIndex i) => HolDocIndex Documents a i where
    unionDocIndex dt1 ix1 dt2 ix2
        | s1 == 0
            = (dt2, ix2)
        | s2 == 0
            = (dt1, ix1)
        | s1 < s2
            = unionDocIndex dt2 ix2 dt1 ix1
        | otherwise
            = (dt, ix)
        where
          dt = unionDocs'    dt1  dt2   -- the unchecked version of union of doctables
          ix = mergeIndexes  ix1  ix2
          s1 = sizeDocs dt1
          s2 = sizeDocs dt2

-- ----------------------------------------------------------------------------

toDocument                      :: (Binary a) => CompressedDoc a -> Document a
toDocument                      = B.decode . BZ.decompress . unCDoc

fromDocument                    :: (Binary a) => Document a -> CompressedDoc a
fromDocument                    = CDoc . BZ.compress . B.encode

mapDocument                     :: (Binary a) => (Document a -> Document a) -> CompressedDoc a -> CompressedDoc a
mapDocument f                   = fromDocument . f . toDocument

toDocMap                        :: (Binary a) => DocIdMap (Document a) -> DocMap a
toDocMap                        = mapDocIdMap fromDocument

fromDocMap                      :: (Binary a) => DocMap a -> DocIdMap (Document a)
fromDocMap                      = mapDocIdMap toDocument

-- ----------------------------------------------------------------------------

instance Binary a => HolDocuments Documents a where
  sizeDocs
      = sizeDocIdMap . idToDoc
  
  lookupById  d i
      = maybe (fail "") return
        . fmap toDocument
        . lookupDocIdMap i
        . idToDoc
        $ d

  lookupByURI d u
      = maybe (fail "") (const $ return i)
        . lookupDocIdMap i
        . idToDoc
        $ d
        where
          i = docToId u

  disjointDocs dt1 dt2
      = nullDocIdMap $ intersectionDocIdMap (idToDoc dt1) (idToDoc dt2)

  unionDocs dt1 dt2
      | disjointDocs dt1 dt2
          = unionDocs' dt1 dt2
      | otherwise
          = error $
            "HashedDocuments.unionDocs: doctables are not disjoint"

  makeEmpty _
      = emptyDocuments

  insertDoc ds d
      = maybe reallyInsert (const (newId, ds)) (lookupById ds newId)
        where
          newId
              = docToId . uri $ d
          d'  = fromDocument d
          reallyInsert
              = rnf d' `seq`                    -- force document compression
                (newId, Documents {idToDoc = insertDocIdMap newId d' $ idToDoc ds})

  updateDoc ds i d
      = rnf d' `seq`                    -- force document compression
        Documents {idToDoc = insertDocIdMap i d' $ idToDoc ds}
      where
        d'                      = fromDocument d

  removeById ds d
      = Documents {idToDoc = deleteDocIdMap d $ idToDoc ds}


  updateDocuments f d
      = Documents {idToDoc = mapDocIdMap (mapDocument f) (idToDoc d)}


  filterDocuments p d
      = Documents {idToDoc = filterDocIdMap (p . toDocument) (idToDoc d)}


  fromMap itd'
      = Documents {idToDoc = toDocMap itd'}

  toMap
      = fromDocMap . idToDoc

-- ----------------------------------------------------------------------------

instance (Binary a, XmlPickler a) => XmlPickler (Documents a) where
    xpickle
        = xpElem "documents" $
          xpWrap convertDoctable $
          xpWrap (fromListDocIdMap, toListDocIdMap) $
          xpList xpDocumentWithId
        where
        convertDoctable
            = (Documents, idToDoc)
        xpDocumentWithId
            = xpElem "doc" $
              xpPair (xpAttr "id" xpDocId) xpickle

-- ----------------------------------------------------------------------------

instance Binary a => Binary (Documents a) where
    put = B.put . idToDoc
    get = fmap Documents B.get

-- ------------------------------------------------------------

instance (Binary a, XmlPickler a) => XmlPickler (CompressedDoc a) where
    xpickle
        = xpWrap (fromDocument , toDocument) $
          xpickle

-- ----------------------------------------------------------------------------

instance Binary a => Binary (CompressedDoc a) where
    put = B.put . unCDoc
    get = B.get >>= return . CDoc

-- ----------------------------------------------------------------------------

instance NFData (CompressedDoc a) where
    rnf (CDoc s)
        = BS.length s `seq` ()

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments :: Documents a
emptyDocuments
    = Documents emptyDocIdMap

unionDocs' :: Documents a -> Documents a -> Documents a
unionDocs' dt1 dt2
    = Documents
      { idToDoc = unionDocIdMap (idToDoc dt1) (idToDoc dt2) }

-- | Create a document table containing a single document.

singleton :: (Binary a) => Document a -> Documents a
singleton d
    = rnf d' `seq`
      Documents {idToDoc = singletonDocIdMap (docToId . uri $ d) d'}
    where
      d' = fromDocument d

-- ------------------------------------------------------------
