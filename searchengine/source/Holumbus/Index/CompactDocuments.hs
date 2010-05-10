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

  -- * Construction
  , emptyDocuments
  , singleton
  , fromMap
  , toMap
  
  -- * Conversion
  , simplify
)
where
import qualified Codec.Compression.BZip	as BZ

import           Control.DeepSeq

import           Data.Binary		( Binary )
import qualified Data.Binary            as B
import		 Data.ByteString.Lazy 	( ByteString )
import qualified Data.ByteString.Lazy 	as BS

import qualified Holumbus.Data.PrefixTree as M

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import           Holumbus.Index.Common

import           Text.XML.HXT.Arrow

-- ----------------------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.

type URIMap			= M.PrefixTree DocId
type DocMap a			= IntMap (CompressedDoc a)

newtype CompressedDoc a		= CDoc { unCDoc :: ByteString }
                                  deriving (Eq, Show)

data Documents a 		= Documents
                                  { idToDoc   :: ! (DocMap a)			-- ^ A mapping from a document id to the document itself.
                                  , docToId   :: ! URIMap                	-- ^ A space efficient mapping from the URI of a document to its id.
                                  , lastDocId :: ! DocId                 	-- ^ The last used document id.
                                  }
                                  deriving (Show)

toDocument			:: (Binary a) => CompressedDoc a -> Document a
toDocument			= B.decode . BZ.decompress . unCDoc

fromDocument			:: (Binary a) => Document a -> CompressedDoc a
fromDocument			= CDoc . BZ.compress . B.encode

mapDocument			:: (Binary a) => (Document a -> Document a) -> CompressedDoc a -> CompressedDoc a
mapDocument f			= fromDocument . f . toDocument

toDocMap			:: (Binary a) => IntMap (Document a) -> DocMap a
toDocMap			= IM.map fromDocument

fromDocMap			:: (Binary a) => DocMap a -> IntMap (Document a)
fromDocMap			= IM.map toDocument

instance Binary a => HolDocuments Documents a where
  sizeDocs d 			= IM.size (idToDoc d)
  
  lookupById  d i 		= maybe (fail "") return . fmap toDocument . IM.lookup i $ idToDoc d
  lookupByURI d u 		= maybe (fail "") return . M.lookup  u $ docToId d

  mergeDocs d1 d2 		= (conflicts, Documents merged (idToDoc2docToId merged) lid)
    where
    (conflicts, merged, lid) 	= IM.foldWithKey checkDoc ([], (idToDoc d1), (lastDocId d1)) (idToDoc d2)
      where
      checkDoc i doc (c, d, l) 	= maybe checkId (\ni -> ((i, ni):c, d, l)) (lookupByURI d1 (uri . toDocument $ doc))
        where
        checkId 		= if IM.member i d
                                  then let
                                       ni = l + 1
                                       in
                                       ((i, ni):c, IM.insert ni doc d, ni)
                                  else (c, IM.insert i doc d, max i l)

  unionDocs dt1 dt2		= Documents
                                  { idToDoc	= IM.union (idToDoc dt1) (idToDoc dt2)
                                  , docToId	= M.union  (docToId dt1) (docToId dt2)
                                  , lastDocId	= lastDocId dt1 `max` lastDocId dt2
                                  }

  makeEmpty _ 			= emptyDocuments

  insertDoc ds d 		= maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds (uri d))
    where
    reallyInsert 		= (newId, Documents newIdToDoc newDocToId newId)
      where
      newIdToDoc 		= IM.insert newId (fromDocument d) (idToDoc ds)
      newDocToId 		= M.insert (uri d) newId (docToId ds)
      newId 			= (lastDocId ds) + 1

  updateDoc ds i d 		= ds 
                                  { idToDoc = IM.insert i (fromDocument d) (idToDoc ds)
                                  , docToId = M.insert (uri d) i (docToId (removeById ds i))
                                  }


  removeById ds d 		= maybe ds reallyRemove (lookupById ds d)
    where
    reallyRemove (Document _ u _) = Documents (IM.delete d (idToDoc ds)) (M.delete u (docToId ds)) (lastDocId ds)

  updateDocuments f d 		= Documents updated (idToDoc2docToId updated) (lastId updated)
    where
    updated 			= IM.map (mapDocument f) (idToDoc d)

  filterDocuments p d 		= Documents filtered (idToDoc2docToId filtered) (lastId filtered)
    where 
    filtered 			= IM.filter (p . toDocument) (idToDoc d)  

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
    newIdToDoc			= IM.foldWithKey (IM.insert . f) IM.empty $ idToDoc d

-- ------------------------------------------------------------

-- Ignoring last document id when testing for equality

instance Eq a => 		Eq (Documents a)
    where
    (==) (Documents i2da d2ia _) (Documents i2db d2ib _)
				= (i2da == i2db)
				  &&
				  (d2ia == d2ib)

instance NFData a => 		NFData (Documents a)
    where
    rnf (Documents i2d d2i lid)	= rnf i2d `seq` rnf d2i `seq` rnf lid `seq` ()

instance (Binary a, XmlPickler a) =>
				XmlPickler (Documents a)
    where
    xpickle 			= xpElem "documents" $
                                  xpWrap convertDoctable $
				  xpWrap (IM.fromList, IM.toList) $
				  xpList xpDocumentWithId
	where
	convertDoctable 	= ( \itd -> Documents itd (idToDoc2docToId itd) (lastId itd)
				  , \(Documents itd _ _) -> itd
                                  )
	xpDocumentWithId 	= xpElem "doc" $
				  xpPair (xpAttr "id" xpPrim) xpickle

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

instance Binary a => 		Binary (CompressedDoc a)
    where
    put				= B.put . unCDoc
    get				= B.get >>= return . CDoc

instance 			NFData (CompressedDoc a)
    where
    rnf (CDoc s)		= BS.length s `seq` ()

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments 			:: Documents a
emptyDocuments 			= Documents IM.empty M.empty 0

-- | Create a document table containing a single document.

singleton 			:: (Binary a) => Document a -> Documents a
singleton d 			= Documents (IM.singleton 1 (fromDocument d)) (M.singleton (uri d) 1) 1

-- | Simplify a document table by transforming the custom information into a string.

simplify 			:: (Binary a, Show a) => Documents a -> Documents String
simplify dt 			= Documents (simple (idToDoc dt)) (docToId dt) (lastDocId dt)
  where
  simple i2d 			= IM.map (fromDocument . (\d -> Document (title d) (uri d) (maybe Nothing (Just . show) (custom d))) . toDocument) i2d

-- | Construct the inverse map from the original map.

idToDoc2docToId 		:: Binary a => DocMap a -> URIMap
idToDoc2docToId 		= IM.foldWithKey (\i d r -> M.insert (uri . toDocument $ d) i r) M.empty

-- | Query the 'idToDoc' part of the document table for the last id.

lastId 				:: DocMap a -> Int
lastId				= maybe 0 (fst . fst) . IM.maxViewWithKey

-- ------------------------------------------------------------
