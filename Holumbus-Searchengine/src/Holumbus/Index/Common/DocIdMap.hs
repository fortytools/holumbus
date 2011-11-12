{-# OPTIONS -XTypeSynonymInstances -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.DocIdMap
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  DocId maps

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.DocIdMap
where

import Control.DeepSeq

import Data.Binary              ( Binary (..) )
import qualified
       Data.Binary              as B
import qualified
       Data.EnumMap             as IM

import Holumbus.Index.Common.DocId

-- import Text.XML.HXT.Core

-- ------------------------------------------------------------

type DocIdMap v                 = IM.EnumMap DocId v

emptyDocIdMap                   :: DocIdMap v
emptyDocIdMap                   = IM.empty

singletonDocIdMap               :: DocId -> v -> DocIdMap v
singletonDocIdMap d v           = insertDocIdMap d v emptyDocIdMap

nullDocIdMap                    :: DocIdMap v -> Bool
nullDocIdMap                    = IM.null

lookupDocIdMap                  :: DocId -> DocIdMap v -> Maybe v
lookupDocIdMap                  = IM.lookup

insertDocIdMap                  :: DocId -> v -> DocIdMap v -> DocIdMap v
insertDocIdMap                  = IM.insert

insertWithDocIdMap              :: (v -> v -> v) -> DocId -> v -> DocIdMap v -> DocIdMap v
insertWithDocIdMap              = IM.insertWith

sizeDocIdMap                    :: DocIdMap v -> Int
sizeDocIdMap                    = IM.size

maxKeyDocIdMap          	:: DocIdMap v -> DocId
maxKeyDocIdMap          	= maybe nullDocId (fst . fst) . IM.maxViewWithKey

differenceDocIdMap             :: DocIdMap v -> DocIdMap v -> DocIdMap v
differenceDocIdMap              = IM.difference

unionWithDocIdMap              :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
unionWithDocIdMap               = IM.unionWith

intersectionWithDocIdMap       :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
intersectionWithDocIdMap       = IM.intersectionWith

unionsWithDocIdMap             :: (v -> v -> v) -> [DocIdMap v] -> DocIdMap v
unionsWithDocIdMap              = IM.unionsWith

mapDocIdMap                     :: (v -> r) -> DocIdMap v -> DocIdMap r
mapDocIdMap                     = IM.map

filterWithKeyDocIdMap           :: (DocId -> v -> Bool) -> DocIdMap v -> DocIdMap v
filterWithKeyDocIdMap           = IM.filterWithKey

mapWithKeyDocIdMap              :: (DocId -> v -> r) -> DocIdMap v -> DocIdMap r
mapWithKeyDocIdMap              = IM.mapWithKey

foldDocIdMap                    :: (v -> b -> b) -> b -> DocIdMap v -> b
foldDocIdMap                    = IM.fold

foldWithKeyDocIdMap             :: (DocId -> v -> b -> b) -> b -> DocIdMap v -> b
foldWithKeyDocIdMap             = IM.foldWithKey

fromListDocIdMap                :: [(DocId, v)] -> DocIdMap v
fromListDocIdMap                = IM.fromList

toListDocIdMap                  :: DocIdMap v -> [(DocId, v)]
toListDocIdMap                  = IM.toList

keysDocIdMap                    :: DocIdMap v -> [DocId]
keysDocIdMap                    = IM.keys

elemsDocIdMap                    :: DocIdMap v -> [v]
elemsDocIdMap                    = IM.elems

instance NFData v => NFData (DocIdMap v) where
    rnf m                       = rnf (IM.toList m)

instance Binary v => Binary (DocIdMap v) where
    put                         = B.put . IM.toList
    get                         = B.get >>= return . IM.fromList

-- ------------------------------------------------------------
