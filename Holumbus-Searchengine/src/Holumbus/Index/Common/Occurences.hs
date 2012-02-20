{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.Occurences
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The Occurences data type

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.Occurences
where

import Control.DeepSeq

import Data.Binary              ( Binary (..) )
import qualified
       Data.Binary              as B

import qualified Data.EnumSet   as IS

import Holumbus.Index.Common.BasicTypes
import Holumbus.Index.Common.DocId
import Holumbus.Index.Common.DocIdMap

import Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | The occurrences in a number of documents.
-- A mapping from document ids to the positions in the document.

type Occurrences        = DocIdMap Positions

-- | Create an empty set of positions.
emptyOccurrences        :: Occurrences
emptyOccurrences        = emptyDocIdMap

-- | Create an empty set of positions.
singletonOccurrence     :: DocId -> Position -> Occurrences
singletonOccurrence d p = insertOccurrence d p emptyDocIdMap

-- | Test on empty set of positions.
nullOccurrences         :: Occurrences -> Bool
nullOccurrences         = nullDocIdMap

-- | Determine the number of positions in a set of occurrences.
sizeOccurrences         :: Occurrences -> Int
sizeOccurrences         = foldDocIdMap ((+) . IS.size) 0

insertOccurrence        :: DocId -> Position -> Occurrences -> Occurrences
insertOccurrence d p    = insertWithDocIdMap IS.union d (singletonPos p)

deleteOccurrence        :: DocId -> Position -> Occurrences -> Occurrences
deleteOccurrence d p    = substractOccurrences (singletonDocIdMap d (singletonPos p))

updateOccurrences       :: (DocId -> DocId) -> Occurrences -> Occurrences
updateOccurrences f     = foldWithKeyDocIdMap
                          (\ d ps res -> insertWithDocIdMap IS.union (f d) ps res) emptyOccurrences

-- | Merge two occurrences.
mergeOccurrences        :: Occurrences -> Occurrences -> Occurrences
mergeOccurrences        = unionWithDocIdMap IS.union

diffOccurrences         :: Occurrences -> Occurrences -> Occurrences
diffOccurrences          = differenceDocIdMap

-- | Substract occurrences from some other occurrences.
substractOccurrences    :: Occurrences -> Occurrences -> Occurrences
substractOccurrences    = differenceWithDocIdMap substractPositions
  where
  substractPositions p1 p2
                        = if IS.null diffPos
                          then Nothing
                          else Just diffPos
      where
      diffPos                   = IS.difference p1 p2

-- | The XML pickler for the occurrences of a word.

xpOccurrences           :: PU Occurrences
xpOccurrences           = xpWrap (fromListDocIdMap, toListDocIdMap)
                                 (xpList xpOccurrence)
  where
  xpOccurrence          = xpElem "doc" $
                          xpPair (xpAttr "idref" xpDocId)
                                 xpPositions

-- ------------------------------------------------------------

-- | The positions of the word in the document.
type Positions                  = IS.EnumSet Position

emptyPos                :: Positions
emptyPos                = IS.empty

singletonPos            :: Position -> Positions
singletonPos            = IS.singleton

memberPos               :: Position -> Positions -> Bool
memberPos               = IS.member

toAscListPos            :: Positions -> [Position]
toAscListPos            = IS.toAscList

fromListPos             :: [Position] -> Positions
fromListPos             = IS.fromList

sizePos                 :: Positions -> Int
sizePos                 = IS.size

unionPos                :: Positions -> Positions -> Positions
unionPos                = IS.union

foldPos                 :: (Position -> r -> r) -> r -> Positions -> r
foldPos                 = IS.fold

-- | The XML pickler for a set of positions.
xpPositions             :: PU Positions
xpPositions             = xpWrap ( IS.fromList . (map read) . words
                                 , unwords . (map show) . IS.toList
                                 ) xpText

instance (NFData v, Enum v) => NFData (IS.EnumSet v) where
    rnf                   = rnf . IS.toList

instance (Binary v, Enum v) => Binary (IS.EnumSet v) where
    put                   = B.put . IS.toList
    get                   = B.get >>= return . IS.fromList

-- ------------------------------------------------------------

