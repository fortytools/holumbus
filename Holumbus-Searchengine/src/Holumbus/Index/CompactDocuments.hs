{-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.CompactDocuments
  Copyright  : Copyright (C) 2007-2010 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: MultiParamTypeClasses FlexibleInstances

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
import qualified Codec.Compression.BZip	as BZ

import           Control.DeepSeq

import           Data.Binary		( Binary )
import qualified Data.Binary            as B
import		 Data.ByteString.Lazy 	( ByteString )
import qualified Data.ByteString.Lazy 	as BS

import qualified Holumbus.Data.PrefixTree as M
import           Holumbus.Index.Common

import           Text.XML.HXT.Core

-- ----------------------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.

type URIMap			= M.PrefixTree DocId
type DocMap a			= DocIdMap (CompressedDoc a)

newtype CompressedDoc a		= CDoc { unCDoc :: ByteString }
                                  deriving (Eq, Show)

data Documents a 		= Documents
                                  { idToDoc   :: ! (DocMap a)	-- ^ A mapping from a document id to
                                                                --   the document itself.
                                  , docToId   :: ! URIMap     	-- ^ A space efficient mapping from
                                                                --   the URI of a document to its id.
                                  , lastDocId :: ! DocId       	-- ^ The last used document id.
                                  }
                                  deriving (Show)

-- ----------------------------------------------------------------------------

instance (Binary a, HolIndex i) => HolDocIndex Documents a i where
    unionDocIndex dt1 ix1 dt2 ix2
        | s1 < s2		= unionDocIndex dt2 ix2 dt1 ix1
        | otherwise		= (dt, ix)
        where
	  dt   			= unionDocs     dt1  dt2s
          ix   			= mergeIndexes  ix1  ix2s

          dt2s 			= editDocIds    add1 dt2
          ix2s 			= updateDocIds' add1 ix2

          add1 			= mkDocId . (+ (theDocId m1)) . theDocId
          m1	 		= maxKeyDocIdMap . toMap $ dt1

          s1			= sizeDocs dt1
          s2			= sizeDocs dt2

-- ----------------------------------------------------------------------------

toDocument			:: (Binary a) => CompressedDoc a -> Document a
toDocument			= B.decode . BZ.decompress . unCDoc

fromDocument			:: (Binary a) => Document a -> CompressedDoc a
fromDocument			= CDoc . BZ.compress . B.encode

mapDocument			:: (Binary a) => (Document a -> Document a) -> CompressedDoc a -> CompressedDoc a
mapDocument f			= fromDocument . f . toDocument

toDocMap			:: (Binary a) => DocIdMap (Document a) -> DocMap a
toDocMap			= mapDocIdMap fromDocument

fromDocMap			:: (Binary a) => DocMap a -> DocIdMap (Document a)
fromDocMap			= mapDocIdMap toDocument

-- ----------------------------------------------------------------------------

instance Binary a => HolDocuments Documents a where
  sizeDocs d 			= sizeDocIdMap (idToDoc d)
  
  lookupById  d i 		= maybe (fail "") return
                                  . fmap toDocument
                                  . lookupDocIdMap i
                                  $ idToDoc d
  lookupByURI d u 		= maybe (fail "") return
                                  . M.lookup  u
                                  $ docToId d

  mergeDocs d1 d2 		= (conflicts, Documents merged (idToDoc2docToId merged) lid)
    where
    (conflicts, merged, lid) 	= foldWithKeyDocIdMap checkDoc ([], (idToDoc d1), (lastDocId d1)) (idToDoc d2)
      where
      checkDoc i doc (c, d, l) 	= maybe checkId (\ni -> ((i, ni):c, d, l))
                                  $ lookupByURI d1 (uri . toDocument $ doc)
        where
        checkId 		= if memberDocIdMap i d
                                  then let ni = incrDocId l in
                                       ((i, ni):c, insertDocIdMap ni doc d, ni)
                                  else (c, insertDocIdMap i doc d, max i l)

  unionDocs dt1 dt2		= Documents
                                  { idToDoc	= unionDocIdMap (idToDoc dt1) (idToDoc dt2)
                                  , docToId	= M.union  (docToId dt1) (docToId dt2)
                                  , lastDocId	= lastDocId dt1 `max` lastDocId dt2
                                  }

  makeEmpty _ 			= emptyDocuments

  insertDoc ds d 		= maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds (uri d))
    where
    reallyInsert 		= (newId, Documents newIdToDoc newDocToId newId)
      where
      newIdToDoc 		= insertDocIdMap newId (fromDocument d) (idToDoc ds)
      newDocToId 		= M.insert (uri d) newId (docToId ds)
      newId 			= incrDocId (lastDocId ds)

  updateDoc ds i d 		= ds 
                                  { idToDoc = insertDocIdMap i (fromDocument d) (idToDoc ds)
                                  , docToId = M.insert (uri d) i (docToId (removeById ds i))
                                  }

  removeById ds d 		= maybe ds reallyRemove (lookupById ds d)
    where
    reallyRemove (Document _ u _)
                                = Documents
                                  (deleteDocIdMap d (idToDoc ds))
                                  (M.delete u (docToId ds))
                                  (lastDocId ds)

  updateDocuments f d 		= Documents updated (idToDoc2docToId updated) (lastId updated)
    where
    updated 			= mapDocIdMap (mapDocument f) (idToDoc d)

  filterDocuments p d 		= Documents filtered (idToDoc2docToId filtered) (lastId filtered)
    where 
    filtered 			= filterDocIdMap (p . toDocument) (idToDoc d)  

  fromMap itd' 			= Documents itd (idToDoc2docToId itd) (lastId itd)
    where
    itd				= toDocMap itd'

  toMap 			= fromDocMap . idToDoc

  editDocIds f d		= Documents
                                  { idToDoc	= newIdToDoc
                                  , docToId	= M.map f $ docToId d
                                  , lastDocId	= lastId newIdToDoc
                                  }
    where
    newIdToDoc			= foldWithKeyDocIdMap (insertDocIdMap . f) emptyDocIdMap
                                  $ idToDoc d

-- ------------------------------------------------------------

-- Ignoring last document id when testing for equality

instance Eq a => 		Eq (Documents a)
    where
    (==) (Documents i2da d2ia _) (Documents i2db d2ib _)
				= (i2da == i2db)
				  &&
				  (d2ia == d2ib)

-- ----------------------------------------------------------------------------

instance NFData a => 		NFData (Documents a)
    where
    rnf (Documents i2d d2i lid)	= rnf i2d `seq` rnf d2i `seq` rnf lid `seq` ()

-- ----------------------------------------------------------------------------

instance (Binary a, XmlPickler a) =>
				XmlPickler (Documents a)
    where
    xpickle 			= xpElem "documents" $
                                  xpWrap convertDoctable $
				  xpWrap (fromListDocIdMap, toListDocIdMap) $
				  xpList xpDocumentWithId
	where
	convertDoctable 	= ( \itd -> Documents itd (idToDoc2docToId itd) (lastId itd)
				  , \(Documents itd _ _) -> itd
                                  )
	xpDocumentWithId 	= xpElem "doc" $
				  xpPair (xpAttr "id" xpDocId) xpickle

-- ----------------------------------------------------------------------------

instance Binary a => 		Binary (Documents a)
    where
    put (Documents i2d _ _) 	= B.put i2d
    get 			= do
                                  i2d <- B.get
                                  return (Documents i2d (idToDoc2docToId i2d) (lastId i2d))

-- ------------------------------------------------------------

instance (Binary a, XmlPickler a) =>
    				XmlPickler (CompressedDoc a)
    where
    xpickle			= xpWrap (fromDocument , toDocument) $
                                  xpickle

-- ----------------------------------------------------------------------------

instance Binary a => 		Binary (CompressedDoc a)
    where
    put				= B.put . unCDoc
    get				= B.get >>= return . CDoc

-- ----------------------------------------------------------------------------

instance 			NFData (CompressedDoc a)
    where
    rnf (CDoc s)		= BS.length s `seq` ()

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments 			:: Documents a
emptyDocuments 			= Documents emptyDocIdMap M.empty nullDocId

-- | Create a document table containing a single document.

singleton 			:: (Binary a) => Document a -> Documents a
singleton d 			= Documents
                                  (singletonDocIdMap firstDocId (fromDocument d))
                                  (M.singleton (uri d) firstDocId)
                                  firstDocId

-- | Simplify a document table by transforming the custom information into a string.

simplify 			:: (Binary a, Show a) => Documents a -> Documents String
simplify dt 			= Documents (simple (idToDoc dt)) (docToId dt) (lastDocId dt)
  where
  simple i2d 			= mapDocIdMap
                                  ( fromDocument
                                    . (\d -> Document (title d) (uri d) (maybe Nothing (Just . show) (custom d)))
                                    . toDocument
                                  ) i2d

-- | Construct the inverse map from the original map.

idToDoc2docToId 		:: Binary a => DocMap a -> URIMap
idToDoc2docToId 		= foldWithKeyDocIdMap
                                  (\i d r -> M.insert (uri . toDocument $ d) i r)
                                  M.empty

-- | Query the 'idToDoc' part of the document table for the last id.

lastId 				:: DocMap a -> DocId
lastId				= maxKeyDocIdMap

-- ------------------------------------------------------------
