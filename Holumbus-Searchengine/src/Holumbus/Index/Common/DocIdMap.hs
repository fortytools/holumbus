{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.DocIdMap
  Copyright  : Copyright (C) 2012 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  DocId maps

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.DocIdMap
    ( DocIdMap
    , emptyDocIdMap
    , singletonDocIdMap
    , nullDocIdMap
    , memberDocIdMap
    , lookupDocIdMap
    , insertDocIdMap
    , deleteDocIdMap
    , insertWithDocIdMap
    , sizeDocIdMap
    , minKeyDocIdMap
    , maxKeyDocIdMap
    , isIntervallDocIdMap
    , unionDocIdMap
    , intersectionDocIdMap
    , differenceDocIdMap
    , unionWithDocIdMap
    , intersectionWithDocIdMap
    , differenceWithDocIdMap
    , unionsWithDocIdMap
    , mapDocIdMap
    , filterDocIdMap
    , filterWithKeyDocIdMap
    , mapWithKeyDocIdMap
    , foldDocIdMap
    , foldWithKeyDocIdMap
    , fromListDocIdMap
    , toListDocIdMap
    , keysDocIdMap
    , elemsDocIdMap
    )
where

import           Control.DeepSeq

import           Data.Binary              ( Binary (..) )
import qualified Data.Binary              as B
import qualified Data.EnumMap             as IM
import           Data.Foldable

import           Holumbus.Index.Common.DocId

-- ------------------------------------------------------------

newtype DocIdMap v              = DIM { unDIM :: IM.EnumMap DocId v }
                                  deriving (Eq, Show, Foldable)

liftDIM                         :: (IM.EnumMap DocId v -> IM.EnumMap DocId r) ->
                                   (DocIdMap v -> DocIdMap r)
liftDIM f                       = DIM . f . unDIM

liftDIM2                        :: (IM.EnumMap DocId v -> IM.EnumMap DocId v -> IM.EnumMap DocId v) ->
                                   (DocIdMap v -> DocIdMap v -> DocIdMap v)
liftDIM2 f x y                  = DIM $ f (unDIM x) (unDIM y)

emptyDocIdMap                   :: DocIdMap v
emptyDocIdMap                   = DIM $ IM.empty

singletonDocIdMap               :: DocId -> v -> DocIdMap v
singletonDocIdMap d v           = insertDocIdMap d v emptyDocIdMap

nullDocIdMap                    :: DocIdMap v -> Bool
nullDocIdMap                    = IM.null . unDIM

memberDocIdMap                  :: DocId -> DocIdMap v -> Bool
memberDocIdMap x                = IM.member x . unDIM

lookupDocIdMap                  :: DocId -> DocIdMap v -> Maybe v
lookupDocIdMap x                = IM.lookup x . unDIM

insertDocIdMap                  :: DocId -> v -> DocIdMap v -> DocIdMap v
insertDocIdMap x y              = liftDIM $ IM.insert x y

deleteDocIdMap                  :: DocId -> DocIdMap v -> DocIdMap v
deleteDocIdMap x                = liftDIM $ IM.delete x

insertWithDocIdMap              :: (v -> v -> v) -> DocId -> v -> DocIdMap v -> DocIdMap v
insertWithDocIdMap f x y        = liftDIM $ IM.insertWith f x y

sizeDocIdMap                    :: DocIdMap v -> Int
sizeDocIdMap                    = IM.size . unDIM

minKeyDocIdMap                  :: DocIdMap v -> DocId
minKeyDocIdMap                  = maybe nullDocId (fst . fst) . IM.minViewWithKey . unDIM

maxKeyDocIdMap                  :: DocIdMap v -> DocId
maxKeyDocIdMap                  = maybe nullDocId (fst . fst) . IM.maxViewWithKey . unDIM

isIntervallDocIdMap             :: DocIdMap v -> Bool
isIntervallDocIdMap m           = nullDocIdMap m
                                  ||
                                  ( fromEnum (theDocId (maxKeyDocIdMap m)) - fromEnum (theDocId (minKeyDocIdMap m))
                                    == sizeDocIdMap m - 1
                                  )

unionDocIdMap                   :: DocIdMap v -> DocIdMap v -> DocIdMap v
unionDocIdMap                   = liftDIM2 $ IM.union

intersectionDocIdMap            :: DocIdMap v -> DocIdMap v -> DocIdMap v
intersectionDocIdMap            = liftDIM2 $ IM.intersection

differenceDocIdMap              :: DocIdMap v -> DocIdMap v -> DocIdMap v
differenceDocIdMap              = liftDIM2 $ IM.difference

unionWithDocIdMap               :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
unionWithDocIdMap f             = liftDIM2 $ IM.unionWith f

intersectionWithDocIdMap        :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
intersectionWithDocIdMap f      = liftDIM2 $ IM.intersectionWith f

differenceWithDocIdMap          :: (v -> v -> Maybe v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
differenceWithDocIdMap f        = liftDIM2 $ IM.differenceWith f

unionsWithDocIdMap              :: (v -> v -> v) -> [DocIdMap v] -> DocIdMap v
unionsWithDocIdMap f            = DIM . IM.unionsWith f . map unDIM

mapDocIdMap                     :: (v -> r) -> DocIdMap v -> DocIdMap r
mapDocIdMap f                   = liftDIM $ IM.map f

filterDocIdMap                  :: (v -> Bool) -> DocIdMap v -> DocIdMap v
filterDocIdMap p                = liftDIM $ IM.filter p

filterWithKeyDocIdMap           :: (DocId -> v -> Bool) -> DocIdMap v -> DocIdMap v
filterWithKeyDocIdMap p         = liftDIM $ IM.filterWithKey p

mapWithKeyDocIdMap              :: (DocId -> v -> r) -> DocIdMap v -> DocIdMap r
mapWithKeyDocIdMap f            = liftDIM $ IM.mapWithKey f

foldDocIdMap                    :: (v -> b -> b) -> b -> DocIdMap v -> b
foldDocIdMap f u                = IM.fold f u . unDIM

foldWithKeyDocIdMap             :: (DocId -> v -> b -> b) -> b -> DocIdMap v -> b
foldWithKeyDocIdMap f u         = IM.foldWithKey f u . unDIM

fromListDocIdMap                :: [(DocId, v)] -> DocIdMap v
fromListDocIdMap                = DIM . IM.fromList

toListDocIdMap                  :: DocIdMap v -> [(DocId, v)]
toListDocIdMap                  = IM.toList . unDIM

keysDocIdMap                    :: DocIdMap v -> [DocId]
keysDocIdMap                    = IM.keys . unDIM

elemsDocIdMap                    :: DocIdMap v -> [v]
elemsDocIdMap                    = IM.elems . unDIM

instance NFData v => NFData (DocIdMap v) where
    rnf                         = rnf . toListDocIdMap

instance Binary v => Binary (DocIdMap v) where
    put                         = B.put . toListDocIdMap
    get                         = B.get >>= return . fromListDocIdMap


-- ------------------------------------------------------------

{-# INLINE liftDIM #-}
{-# INLINE liftDIM2 #-}
{-# INLINE emptyDocIdMap #-}
{-# INLINE singletonDocIdMap #-}
{-# INLINE nullDocIdMap #-}
{-# INLINE memberDocIdMap #-}
{-# INLINE lookupDocIdMap #-}
{-# INLINE insertDocIdMap #-}
{-# INLINE deleteDocIdMap #-}
{-# INLINE insertWithDocIdMap #-}
{-# INLINE sizeDocIdMap #-}
{-# INLINE minKeyDocIdMap #-}
{-# INLINE maxKeyDocIdMap #-}
{-# INLINE isIntervallDocIdMap #-}
{-# INLINE unionDocIdMap #-}
{-# INLINE differenceDocIdMap #-}
{-# INLINE unionWithDocIdMap #-}
{-# INLINE intersectionWithDocIdMap #-}
{-# INLINE differenceWithDocIdMap #-}
{-# INLINE unionsWithDocIdMap #-}
{-# INLINE mapDocIdMap #-}
{-# INLINE filterDocIdMap #-}
{-# INLINE filterWithKeyDocIdMap #-}
{-# INLINE mapWithKeyDocIdMap #-}
{-# INLINE foldDocIdMap #-}
{-# INLINE foldWithKeyDocIdMap #-}
{-# INLINE fromListDocIdMap #-}
{-# INLINE toListDocIdMap #-}
{-# INLINE keysDocIdMap #-}
{-# INLINE elemsDocIdMap #-}

-- ------------------------------------------------------------
