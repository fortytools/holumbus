{-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses #-}

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

import           Data.Binary				( Binary )
import qualified Data.Binary            		as B
import qualified Data.IntMap as IM

import           Holumbus.Index.Common
import qualified Holumbus.Index.CompactDocuments	as CD

import           Text.XML.HXT.Arrow

-- ----------------------------------------------------------------------------

-- | The table to store the document descriptions

newtype SmallDocuments a	= SmallDocuments
                                  { idToSmallDoc   :: CD.DocMap a		-- ^ A mapping from a document id to the document itself.
                                  }

-- ----------------------------------------------------------------------------

instance Binary a => HolDocuments SmallDocuments a where
  sizeDocs 			= IM.size . idToSmallDoc
  
  lookupById  d i 		= maybe (fail "") return . fmap CD.toDocument . IM.lookup i . idToSmallDoc $ d

  makeEmpty _ 			= emptyDocuments

  lookupByURI	 		= error "Not yet implemented"
  mergeDocs	 		= error "Not yet implemented"
  unionDocs			= error "Not yet implemented"
  insertDoc	 		= error "Not yet implemented"
  updateDoc	 		= error "Not yet implemented"
  removeById	 		= error "Not yet implemented"
  updateDocuments		= error "Not yet implemented"
  filterDocuments 		= error "Not yet implemented"
  editDocIds			= error "Not yet implemented"

  fromMap 			= SmallDocuments . CD.toDocMap
  toMap 			= CD.fromDocMap . idToSmallDoc

-- ----------------------------------------------------------------------------

instance NFData a => 		NFData (SmallDocuments a)
    where
    rnf (SmallDocuments i2d)	= rnf i2d `seq` ()

-- ----------------------------------------------------------------------------

instance (Binary a, XmlPickler a) =>
				XmlPickler (SmallDocuments a)
    where
    xpickle 			= xpElem "documents" $
                                  xpWrap convertDoctable $
				  xpWrap (IM.fromList, IM.toList) $
				  xpList xpDocumentWithId
	where
	convertDoctable 	= ( SmallDocuments
				  , idToSmallDoc
                                  )
	xpDocumentWithId 	= xpElem "doc" $
				  xpPair (xpAttr "id" xpPrim) xpickle

-- ----------------------------------------------------------------------------

instance Binary a => 		Binary (SmallDocuments a)
    where
    put (SmallDocuments i2d) 	= B.put i2d
    get 			= do
                                  i2d <- B.get
                                  return $ SmallDocuments i2d

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments 			:: SmallDocuments a
emptyDocuments 			= SmallDocuments IM.empty

-- | Create a document table containing a single document.

singleton 			:: (Binary a) => Document a -> SmallDocuments a
singleton d 			= SmallDocuments (IM.singleton 1 (CD.fromDocument d))

-- | Convert a Compact document table into a small compact document table.
-- Called at the end of building an index

docTable2smallDocTable		:: CD.Documents a -> SmallDocuments a
docTable2smallDocTable		=  SmallDocuments . CD.idToDoc

-- ------------------------------------------------------------
