{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Inverted.PrefixMem
  Copyright  : Copyright (C) 2007 - 2009 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT
  
  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: portable
  
  A variant of the Inverted.Memory index with an optimized prefix tree
  instead of a trie as central data structure. This version should be
  more space efficient as the trie and more runtime efficient when combining
  whole indexes.

  For switching from Memory to this module, only the import has to be modified

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted.PrefixMem 
    (
     -- * Inverted index types
     Inverted (..)
    , Parts
    , Part
  
    -- * Construction
    , singleton
    , emptyInverted
    )
where

import           Control.DeepSeq

import           Data.Binary                    hiding (Word)
import           Data.Function
import           Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe

import qualified Holumbus.Data.PrefixTree       as PT

import           Holumbus.Index.Common
import           Holumbus.Index.Compression

import           Text.XML.HXT.Core

-- ----------------------------------------------------------------------------

-- | The index consists of a table which maps documents to ids and a number of index parts.

newtype Inverted        = Inverted { indexParts :: Parts } 
                          deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.

type Parts              = Map Context Part

-- | The index part is the real inverted index. Words are mapped to their occurrences.

type Part               = PT.PrefixTree CompressedOccurrences

-- ----------------------------------------------------------------------------

instance HolIndex Inverted where
  sizeWords                     = M.fold ((+) . PT.size) 0 . indexParts
  contexts                      = map fst . M.toList . indexParts

  allWords i c                  = map (\(w, o) -> (w, inflateOcc o)) $ PT.toList $ getPart c i
  prefixCase i c q              = map (\(w, o) -> (w, inflateOcc o)) $ PT.prefixFindWithKey q $ getPart c i
  prefixNoCase i c q            = map (\(w, o) -> (w, inflateOcc o)) $ PT.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase i c q              = map (\    o  -> (q, inflateOcc o)) $ maybeToList (PT.lookup q $ getPart c i)
  lookupNoCase i c q            = map (\(w, o) -> (w, inflateOcc o)) $ PT.lookupNoCase q $ getPart c i

  mergeIndexes i1 i2            = Inverted (mergeParts (indexParts i1) (indexParts i2))
  substractIndexes i1 i2        = Inverted (substractParts (indexParts i1) (indexParts i2))

  insertOccurrences c w o i     = mergeIndexes (singleton c w o) i
  deleteOccurrences c w o i     = substractIndexes i (singleton c w o)

  splitByContexts (Inverted ps) = splitInternal (map (uncurry annotate) . M.toList $ ps)
    where
    annotate c p                = let
                                  i = Inverted (M.singleton c p)
                                  in
                                  (sizeWords i, i)

  splitByDocuments i            = splitInternal ( map convert $
                                                  toListDocIdMap $
                                                  unionsWithDocIdMap unionDocs' docResults
                                                )
    where
    unionDocs'                  = M.unionWith (M.unionWith unionPos)
    docResults                  = map (\c -> resultByDocument c (allWords i c)) (contexts i)
    convert (d, cs)             = foldl' makeIndex (0, emptyInverted) (M.toList cs)
      where
      makeIndex r (c, ws)       = foldl' makeOcc r (M.toList ws)
        where
        makeOcc (rs, ri) (w, p) = (sizePos p + rs , insertOccurrences c w (singletonDocIdMap d p) ri)

  splitByWords i                = splitInternal indexes
    where
    indexes                     = map convert $
                                  M.toList $
                                  M.unionsWith (M.unionWith mergeOccurrences) wordResults
      where
      wordResults               = map (\c -> resultByWord c (allWords i c)) (contexts i)
      convert (w, cs)           = foldl' makeIndex (0, emptyInverted) (M.toList cs)
        where
        makeIndex (rs, ri) (c, o) = (rs + sizeOccurrences o, insertOccurrences c w o ri)

  updateDocIds f (Inverted parts)
                                = Inverted (M.mapWithKey updatePart parts)
    where
    updatePart c p              = PT.mapWithKey
                                  (\w o -> foldWithKeyDocIdMap (updateDocument c w) emptyDocIdMap o) p
    updateDocument c w d p r    = insertWithDocIdMap mergePositions (f c w d) p r
      where
      mergePositions p1 p2      = deflatePos $ unionPos (inflatePos p1) (inflatePos p2)

  updateDocIds' f
                                = Inverted . M.map updatePart . indexParts
    where
    updatePart                  = PT.map updateOcc
    updateOcc                   = foldWithKeyDocIdMap updateId emptyDocIdMap
    updateId                    = insertDocIdMap . f

  toList i                      = concat $ map convertPart $ M.toList (indexParts i) 
    where convertPart (c,p)     = map (\(w, o) -> (c, w, inflateOcc o)) $
                                  PT.toList $
                                  p

-- ----------------------------------------------------------------------------

instance NFData Inverted where
    rnf                         = rnf . indexParts

-- ----------------------------------------------------------------------------

instance XmlPickler Inverted where
  xpickle                       =  xpElem "indexes" $
                                   xpWrap (\p -> Inverted p, \(Inverted p) -> p) xpParts

-- | The XML pickler for the index parts.
xpParts                         :: PU Parts
xpParts                         = xpWrap (M.fromList, M.toList) (xpList xpContext)
  where
  xpContext                     = xpElem "part" (xpPair (xpAttr "id" xpText) xpPart)

-- | The XML pickler for a single part.
xpPart                          :: PU Part
xpPart                          = xpElem "index" (xpWrap (PT.fromList, PT.toList) (xpList xpWord))
  where
  xpWord                        = xpElem "word" $
                                  xpPair (xpAttr "w" xpText)
                                         (xpWrap (deflateOcc, inflateOcc) xpOccurrences)

-- ----------------------------------------------------------------------------

instance Binary Inverted where
    put                         = put . indexParts
    get                         = get >>= return . Inverted

-- ----------------------------------------------------------------------------

-- | Create an empty index.
emptyInverted                   :: Inverted
emptyInverted                   = Inverted M.empty
                  
-- | Create an index with just one word in one context.
singleton                       :: Context -> String -> Occurrences -> Inverted
singleton c w o                 = Inverted (M.singleton c (PT.singleton w (deflateOcc o)))

-- | Merge two sets of index parts.
mergeParts                      :: Parts -> Parts -> Parts
mergeParts                      = M.unionWith mergePart

-- | Merge two index parts.
mergePart                       :: Part -> Part -> Part
mergePart                       = PT.unionWith mergeDiffLists
  where
  mergeDiffLists o1 o2          = deflateOcc $
                                  mergeOccurrences (inflateOcc o1) (inflateOcc o2)

-- | Substract a set of index parts from another.
substractParts                  :: Parts -> Parts -> Parts
substractParts                  = M.differenceWith substractPart

-- | Substract one index part from another.
substractPart                   :: Part -> Part -> Maybe Part
substractPart p1 p2             = if PT.null diffPart then Nothing else Just diffPart
  where
  diffPart                      = PT.differenceWith substractDiffLists p1 p2
    where
    substractDiffLists o1 o2    = if diffOcc == emptyOccurrences then Nothing else Just (deflateOcc diffOcc)
      where
      diffOcc                   = substractOccurrences (inflateOcc o1) (inflateOcc o2)

-- | Internal split function used by the split functions from the HolIndex interface (above).
splitInternal                   :: [(Int, Inverted)] -> Int -> [Inverted]
splitInternal inp n             = allocate mergeIndexes stack buckets
  where
  buckets                       = zipWith const (createBuckets n) stack
  stack                         = reverse (sortBy (compare `on` fst) inp)

-- | Allocates values from the first list to the buckets in the second list.
allocate                        :: (a -> a -> a) -> [(Int, a)] -> [(Int, a)] -> [a]
allocate _ _ []                 = []
allocate _ [] ys                = map snd ys
allocate f (x:xs) (y:ys)        = allocate f xs (sortBy (compare `on` fst) ((combine x y):ys))
  where
  combine (s1, v1) (s2, v2)     = (s1 + s2, f v1 v2)

-- | Create empty buckets for allocating indexes.  
createBuckets                   :: Int -> [(Int, Inverted)]
createBuckets n                 = (replicate n (0, emptyInverted))
  
-- | Return a part of the index for a given context.
getPart                         :: Context -> Inverted -> Part
getPart c i                     = fromMaybe PT.empty (M.lookup c $ indexParts i)

-- ----------------------------------------------------------------------------
