{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.CompactDocuments
  Copyright  : Copyright (C) 2007- Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental

  A more space efficient substitute for Holumbus.Index.Documents
-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.CompactDocuments 
(
  -- * Documents type
  Documents (..)
  , CompressedDoc(..)
  , DocMap
  , URIMap

  -- * Construction
  , emptyDocuments
  , singleton
  
  -- * Conversion
  , simplify
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
import           Data.Maybe             ( fromJust )

import qualified Holumbus.Data.PrefixTree as M
import           Holumbus.Index.Common

import           Text.XML.HXT.Core

-- ----------------------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.

type URIMap                     = M.PrefixTree DocId
type DocMap a                   = DocIdMap (CompressedDoc a)

newtype CompressedDoc a         = CDoc { unCDoc :: ByteString }
                                  deriving (Eq, Show)

data Documents a                = Documents
                                  { idToDoc   :: ! (DocMap a)   -- ^ A mapping from a document id to
                                                                --   the document itself.
                                  , docToId   :: ! URIMap       -- ^ A space efficient mapping from
                                                                --   the URI of a document to its id.
                                  , lastDocId :: ! DocId        -- ^ The last used document id.
                                  }
                                  deriving (Show)

-- ----------------------------------------------------------------------------

instance (Binary a, HolIndex i) => HolDocIndex Documents a i where
    defragmentDocIndex dt ix  = (dt1, ix1)
        where
          dt1                   = editDocIds editId dt
          ix1                   = updateDocIds' editId ix
          editId i              = fromJust . lookupDocIdMap i $ idMap
          idMap                 = fromListDocIdMap . flip zip (map mkDocId [1..]) . keysDocIdMap . toMap $ dt

    unionDocIndex dt1 ix1 dt2 ix2
        | s1 == 0               = (dt2, ix2)
        | s2 == 0               = (dt1, ix1)
        | s1 < s2               = unionDocIndex dt2 ix2 dt1 ix1
        | otherwise             = (dt, ix)
        where
          dt                    = unionDocs     dt1  dt2s
          ix                    = mergeIndexes  ix1  ix2s

          dt2s                  = editDocIds    add1 dt2
          ix2s                  = updateDocIds' add1 ix2

          add1                  = addDocId disp 
          max1                  = maxKeyDocIdMap . toMap $ dt1
          min2                  = minKeyDocIdMap . toMap $ dt2
          disp                  = incrDocId $ subDocId max1 min2

          s1                    = sizeDocs dt1
          s2                    = sizeDocs dt2

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
  sizeDocs d                    = sizeDocIdMap (idToDoc d)
  
  lookupById  d i               = maybe (fail "") return
                                  . fmap toDocument
                                  . lookupDocIdMap i
                                  $ idToDoc d
  lookupByURI d u               = maybe (fail "") return
                                  . M.lookup  u
                                  $ docToId d

  -- this is a sufficient test, but if the doc ids don't form an intervall
  -- it may be too restrictive

  disjointDocs dt1 dt2
      | nullDocs dt1
        ||
        nullDocs dt2            = True
      | otherwise               = disjoint ( minKeyDocIdMap . idToDoc $ dt1
                                           , maxKeyDocIdMap . idToDoc $ dt1
                                           )
                                           ( minKeyDocIdMap . idToDoc $ dt2
                                           , maxKeyDocIdMap . idToDoc $ dt2
                                           )
      where
        disjoint p1@(x1, y1) p2@(x2, _y2)
            | x1 <= x2          = y1 < x2
            | otherwise         = disjoint p2 p1

  unionDocs dt1 dt2
      | disjointDocs dt1 dt2    = Documents
                                  { idToDoc     = unionDocIdMap (idToDoc dt1) (idToDoc dt2)
                                  , docToId     = M.union  (docToId dt1) (docToId dt2)
                                  , lastDocId   = lastDocId dt1 `max` lastDocId dt2
                                  }
      | otherwise               = error $
                                  "unionDocs: doctables are not disjoint: " ++
                                  show (didIntervall dt1) ++ ", " ++ show (didIntervall dt2)
      where
       didIntervall dt          = ( minKeyDocIdMap . idToDoc $ dt
                                  , maxKeyDocIdMap . idToDoc $ dt
                                  )

  makeEmpty _                   = emptyDocuments

  insertDoc ds d                = maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds (uri d))
    where
    d'                          = fromDocument d
    reallyInsert                = rnf d' `seq`                  -- force document compression
                                  (newId, Documents newIdToDoc newDocToId newId)
      where
      newIdToDoc                = insertDocIdMap newId d' (idToDoc ds)
      newDocToId                = M.insert (uri d) newId  (docToId ds)
      newId                     = incrDocId (lastDocId ds)

  updateDoc ds i d              = rnf d' `seq`                  -- force document compression
                                  ds 
                                  { idToDoc = insertDocIdMap i d' (idToDoc ds)
                                  , docToId = M.insert (uri d) i (docToId (removeById ds i))
                                  }
      where
        d'                      = fromDocument d

  removeById ds d               = maybe ds reallyRemove (lookupById ds d)
    where
    reallyRemove (Document _ u _)
                                = Documents
                                  (deleteDocIdMap d (idToDoc ds))
                                  (M.delete u (docToId ds))
                                  (lastDocId ds)

  updateDocuments f d           = Documents updated (idToDoc2docToId updated) (lastId updated)
    where
    updated                     = mapDocIdMap (mapDocument f) (idToDoc d)

  filterDocuments p d           = Documents filtered (idToDoc2docToId filtered) (lastId filtered)
    where 
    filtered                    = filterDocIdMap (p . toDocument) (idToDoc d)  

  fromMap itd'                  = Documents itd (idToDoc2docToId itd) (lastId itd)
    where
    itd                         = toDocMap itd'

  toMap                         = fromDocMap . idToDoc

  editDocIds f d                = Documents
                                  { idToDoc     = newIdToDoc
                                  , docToId     = M.map f $ docToId d
                                  , lastDocId   = lastId newIdToDoc
                                  }
    where
    newIdToDoc                  = foldWithKeyDocIdMap (insertDocIdMap . f) emptyDocIdMap
                                  $ idToDoc d

-- ------------------------------------------------------------

-- Ignoring last document id when testing for equality

instance Eq a =>                Eq (Documents a)
    where
    (==) (Documents i2da d2ia _) (Documents i2db d2ib _)
                                = (i2da == i2db)
                                  &&
                                  (d2ia == d2ib)

-- ----------------------------------------------------------------------------

instance NFData a =>            NFData (Documents a)
    where
    rnf (Documents i2d d2i lid) = rnf i2d `seq` rnf d2i `seq` rnf lid

-- ----------------------------------------------------------------------------

instance (Binary a, XmlPickler a) =>
                                XmlPickler (Documents a)
    where
    xpickle                     = xpElem "documents" $
                                  xpWrap convertDoctable $
                                  xpWrap (fromListDocIdMap, toListDocIdMap) $
                                  xpList xpDocumentWithId
        where
        convertDoctable         = ( \ itd -> Documents itd (idToDoc2docToId itd) (lastId itd)
                                  , \ (Documents itd _ _) -> itd
                                  )
        xpDocumentWithId        = xpElem "doc" $
                                  xpPair (xpAttr "id" xpDocId) xpickle

-- ----------------------------------------------------------------------------

instance Binary a =>            Binary (Documents a)
    where
    put (Documents i2d _ lid)   = B.put lid >> B.put i2d
    get                         = do lid <- B.get
                                     i2d <- B.get
                                     return (Documents i2d (idToDoc2docToId i2d) lid)

-- ------------------------------------------------------------

instance (Binary a, XmlPickler a) =>
                                XmlPickler (CompressedDoc a)
    where
    xpickle                     = xpWrap (fromDocument , toDocument) $
                                  xpickle

-- ----------------------------------------------------------------------------

instance Binary a =>            Binary (CompressedDoc a)
    where
    put                         = B.put . unCDoc
    get                         = B.get >>= return . CDoc

-- ----------------------------------------------------------------------------

instance                        NFData (CompressedDoc a)
    where
    rnf (CDoc s)                = BS.length s `seq` ()

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments                  :: Documents a
emptyDocuments                  = Documents emptyDocIdMap M.empty nullDocId

-- | Create a document table containing a single document.

singleton                       :: (Binary a) => Document a -> Documents a
singleton d                     = rnf d' `seq`
                                  Documents
                                  (singletonDocIdMap firstDocId d')
                                  (M.singleton (uri d) firstDocId)
                                  firstDocId
    where
      d'                        = fromDocument d

-- | Simplify a document table by transforming the custom information into a string.

simplify                        :: (Binary a, Show a) => Documents a -> Documents String
simplify dt                     = Documents (simple (idToDoc dt)) (docToId dt) (lastDocId dt)
  where
  simple i2d                    = mapDocIdMap
                                  ( fromDocument
                                    . (\d -> Document (title d) (uri d) (maybe Nothing (Just . show) (custom d)))
                                    . toDocument
                                  ) i2d

-- | Construct the inverse map from the original map.

idToDoc2docToId                 :: Binary a => DocMap a -> URIMap
idToDoc2docToId                 = foldWithKeyDocIdMap
                                  (\i d r -> M.insert (uri . toDocument $ d) i r)
                                  M.empty

-- | Query the 'idToDoc' part of the document table for the last id.

lastId                          :: DocMap a -> DocId
lastId                          = maxKeyDocIdMap

-- ------------------------------------------------------------
