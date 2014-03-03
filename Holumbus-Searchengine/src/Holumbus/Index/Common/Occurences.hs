{-# LANGUAGE BangPatterns #-}
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

import           Control.Applicative              ((<$>))
import qualified Data.Binary                      as B
import qualified Data.IntSet                      as IS
import qualified Data.IntSet.Cache                as IS

import qualified Debug.Trace                      as DT

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap

import           Text.XML.HXT.Core

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
type Positions                  = IS.IntSet

emptyPos                :: Positions
emptyPos                = IS.empty

singletonPos            :: Position -> Positions
singletonPos            = IS.cacheAt

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

{-# INLINE emptyPos     #-}
{-# INLINE singletonPos #-}
{-# INLINE memberPos    #-}
{-# INLINE toAscListPos #-}
{-# INLINE fromListPos  #-}
{-# INLINE sizePos      #-}
{-# INLINE unionPos     #-}
{-# INLINE foldPos      #-}

-- ------------------------------------------------------------

newtype WrappedPositions = WPos {unWPos :: Positions}

instance B.Binary WrappedPositions where
    put = B.put . IS.toList . unWPos
    get = (WPos . IS.unions . map IS.cacheAt) <$> B.get

newtype WrappedOccs = WOccs {unWOccs :: Occurrences}

instance B.Binary WrappedOccs where
    put = B.put . mapDocIdMap WPos . unWOccs
    get = (WOccs . mapDocIdMap unWPos) <$> B.get

-- ------------------------------------------------------------
-- Just for space performance stats

sizeOccPos :: Occurrences -> (Int, Int)
sizeOccPos os
    = foldDocIdMap (\ ps (!dc, !pc) -> (dc + 1, pc + IS.size ps)) (0, 0) os


traceOccPos :: Occurrences -> Occurrences
traceOccPos os
    = DT.trace msg os
    where
      _sc@(!dc, !pc) = sizeOccPos os
      v0 | dc == pc  = show $ foldDocIdMap (\ ps res -> IS.elems ps ++ res) [] os
         | otherwise = show $ foldDocIdMap (\ ps res -> IS.elems ps :  res) [] os
      msg  = "traceOccPos: " ++ v0

-- ------------------------------------------------------------
