{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.CompactDocuments
  Copyright  : Copyright (C) 2007-2010 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: MultiParamTypeClasses FlexibleInstances

  A simple version of Holumbus.Index.Documents.
  This implementation is only for reading a document table in the search part of an application.
  The mapping of URIs to DocIds is only required during index building, not when accessing the index.
  So this 2. mapping is removed in this implementation for saving space
-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.CompactSmallDocuments 
(
  -- * Documents type
  SmallDocuments (..)

  -- * Construction
  , emptyDocuments
  , singleton

  -- * Conversion
  , docTable2smallDocTable
)
where

import           Control.DeepSeq

import           Data.Binary                            ( Binary )
import qualified Data.Binary                            as B

import           Holumbus.Index.Common
import qualified Holumbus.Index.CompactDocuments        as CD

import           Text.XML.HXT.Core

-- ----------------------------------------------------------------------------

-- | The table to store the document descriptions
--
-- This table does not contain the reverse map from URIs do DocIds,
-- this reverse map is only needed when crawling, not for searching the index.
-- As a consequence, most of the indes operations are not implemented
--
-- see also 'Holumbus.Index.CompactDocuments.Documents' data type

newtype SmallDocuments a        = SmallDocuments
                                  { idToSmallDoc   :: CD.DocMap a -- ^ A mapping from a doc id
                                                                  --   to the document itself.
                                  }

-- ----------------------------------------------------------------------------

instance (Binary a, HolIndex i) => HolDocIndex SmallDocuments a i where
    defragmentDocIndex          = notImpl
{-
    defragmentDocIndex dt ix    = (dt1, ix1)
        where
          dt1                   = editDocIds editId dt
          ix1                   = updateDocIds' editId ix
          editId i              = fromJust . lookupDocIdMap i $ idMap
          idMap                 = fromListDocIdMap . flip zip (map mkDocId [1..]) . keysDocIdMap . toMap $ dt
-}

    unionDocIndex dt1 ix1 dt2 ix2
                                = (dt, ix)
        where
          dt                    = unionDocs     dt1  dt2
          ix                    = mergeIndexes  ix1  ix2

-- ----------------------------------------------------------------------------

instance Binary a => HolDocuments SmallDocuments a where
  sizeDocs                      = sizeDocIdMap . idToSmallDoc
  
  lookupById  d i               = maybe (fail "") return
                                  . fmap CD.toDocument
                                  . lookupDocIdMap i
                                  . idToSmallDoc
                                  $ d

  makeEmpty _                   = emptyDocuments

  -- this is a sufficient test, but if the doc ids don't form an intervall
  -- it may be too strict

  disjointDocs dt1 dt2
      | nullDocs dt1
        ||
        nullDocs dt2            = True
      | otherwise               = disjoint ( minKeyDocIdMap . idToSmallDoc $ dt1
                                           , maxKeyDocIdMap . idToSmallDoc $ dt1
                                           )
                                           ( minKeyDocIdMap . idToSmallDoc $ dt2
                                           , maxKeyDocIdMap . idToSmallDoc $ dt2
                                           )
      where
        disjoint p1@(x1, y1) p2@(x2, _y2)
            | x1 <= x2          = y1 < x2
            | otherwise         = disjoint p2 p1

  unionDocs dt1 dt2
      | disjointDocs dt1 dt2    = SmallDocuments
                                  { idToSmallDoc = unionDocIdMap (idToSmallDoc dt1) (idToSmallDoc dt2)
                                  }
      | otherwise               = error $
                                  "unionDocs: doctables are not disjoint: " ++
                                  show (didIntervall dt1) ++ ", " ++ show (didIntervall dt2)
      where
       didIntervall dt          = ( minKeyDocIdMap . idToSmallDoc $ dt
                                  , maxKeyDocIdMap . idToSmallDoc $ dt
                                  )

  editDocIds f d                = SmallDocuments
                                  { idToSmallDoc = newIdToDoc
                                  }
      where
        newIdToDoc              = foldWithKeyDocIdMap (insertDocIdMap . f) emptyDocIdMap
                                  $ idToSmallDoc d

  fromMap                       = SmallDocuments . CD.toDocMap
  toMap                         = CD.fromDocMap . idToSmallDoc

  -- only lookup by doc id, union and defragment ops are implemented
  -- the others are not needed when merging or searching the doc indexes

  lookupByURI                   = notImpl

  insertDoc                     = notImpl
  updateDoc                     = notImpl
  removeById                    = notImpl
  updateDocuments               = notImpl
  filterDocuments               = notImpl

-- ----------------------------------------------------------------------------

instance NFData a =>            NFData (SmallDocuments a)
    where
    rnf (SmallDocuments i2d)    = rnf i2d

-- ----------------------------------------------------------------------------

instance (Binary a, XmlPickler a) =>
                                XmlPickler (SmallDocuments a)
    where
    xpickle                     = xpElem "documents" $
                                  xpWrap convertDoctable $
                                  xpWrap (fromListDocIdMap, toListDocIdMap) $
                                  xpList xpDocumentWithId
        where
        convertDoctable         = ( SmallDocuments
                                  , idToSmallDoc
                                  )
        xpDocumentWithId        = xpElem "doc" $
                                  xpPair (xpAttr "id" xpDocId) xpickle

-- ----------------------------------------------------------------------------

instance Binary a =>            Binary (SmallDocuments a)
    where
    put (SmallDocuments i2d)    = B.put i2d
    get                         = do
                                  i2d <- B.get
                                  return $ SmallDocuments i2d

-- ------------------------------------------------------------

notImpl                         :: a
notImpl                         = error "operation not implemented for SmallDocuments data type"

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments                  :: SmallDocuments a
emptyDocuments                  = SmallDocuments emptyDocIdMap

-- | Create a document table containing a single document.

singleton                       :: (Binary a) => Document a -> SmallDocuments a
singleton d                     = SmallDocuments (singletonDocIdMap firstDocId (CD.fromDocument d))

-- | Convert a Compact document table into a small compact document table.
-- Called at the end of building an index

docTable2smallDocTable          :: CD.Documents a -> SmallDocuments a
docTable2smallDocTable          =  SmallDocuments . CD.idToDoc

-- ------------------------------------------------------------
