-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Inverted
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2
  
  The inverted index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted 
(
  -- * Inverted index types
  InvIndex (..)
  
  -- * Construction
  , singleton
  , emptyInverted
   
  -- *  Compression
  , deflate
  , inflate
)
where

import Text.XML.HXT.Arrow

import Data.Function
import Data.Maybe
import Data.Binary
import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Data.DiffList (DiffList)
import qualified Holumbus.Data.DiffList as DL

import Holumbus.Index.Common

import Control.Parallel.Strategies

-- | The index consists of a table which maps documents to ids and a number of index parts.
newtype InvIndex = InvIndex { indexParts :: Parts } deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to their occurrences.
type Part        = StrMap (IntMap DiffList)

instance HolIndex InvIndex where
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  contexts = map fst . M.toList . indexParts

  allWords i c = map (\(w, o) -> (w, inflate o)) $ SM.toList $ getPart c i
  prefixCase i c q = map (\(w, o) -> (w, inflate o)) $ SM.prefixFindWithKey q $ getPart c i
  prefixNoCase i c q = map (\(w, o) -> (w, inflate o)) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase i c q = map (\o -> (q, inflate o)) $ maybeToList (SM.lookup q $ getPart c i)
  lookupNoCase i c q = map (\(w, o) -> (w, inflate o)) $ SM.lookupNoCase q $ getPart c i

  mergeIndexes i1 i2 = InvIndex (mergeParts (indexParts i1) (indexParts i2))
  substractIndexes _ _ = undefined

  insertOccurrences c w o i = mergeIndexes (singleton c w o) i
  deleteOccurrences c w o i = substractIndexes i (singleton c w o)

  splitByContexts (InvIndex parts) n = allocate mergeIndexes stack buckets 
    where
    buckets = zipWith const (createBuckets n) stack
    stack = reverse (L.sortBy (compare `on` fst) (map annotate $ M.toList parts))
      where
      annotate (c, p) = let i = InvIndex (M.singleton c p) in (sizeWords i, i)

  splitByDocuments _ _ = undefined

  splitByWords i n = allocate mergeIndexes stack buckets
    where
    buckets = zipWith const (createBuckets n) stack
    stack = reverse (L.sortBy (compare `on` fst) (map annotate $ foldr (\c r -> r ++ map (\(w, o) -> (w, c, o)) (allWords i c)) [] (contexts i)))
      where
      annotate (w, c, o) = (sizeOccurrences o, singleton c w o)

  updateDocuments f (InvIndex parts) = InvIndex (M.mapWithKey updatePart parts)
    where
    updatePart c p = SM.mapWithKey (\w o -> IM.foldWithKey (updateDocument c w) IM.empty o) p
    updateDocument c w d p r = IM.insertWith mergePositions (f c w d) p r
      where
      mergePositions p1 p2 = DL.fromIntSet $ IS.union (DL.toIntSet p1) (DL.toIntSet p2)

instance NFData InvIndex where
  rnf (InvIndex parts) = rnf parts

instance XmlPickler InvIndex where
  xpickle =  xpElem "indexes" $ xpWrap (\p -> InvIndex p, \(InvIndex p) -> p) xpParts

instance Binary InvIndex where
  put (InvIndex parts) = put parts
  get = do parts <- get
           return (InvIndex parts)

-- | Create an index with just one word in one context.
singleton :: Context -> String -> Occurrences -> InvIndex
singleton c w o = InvIndex (M.singleton c (SM.singleton w (deflate o)))

-- | Merge two sets of index parts.
mergeParts :: Parts -> Parts -> Parts
mergeParts = M.unionWith mergePart

-- | Merge two index parts.
mergePart :: Part -> Part -> Part
mergePart = SM.foldWithKey (mergeWords)
  where
  mergeWords w o p = SM.insertWith mergeDiffLists w o p
    where
    mergeDiffLists o1 o2 = deflate $ mergeOccurrences (inflate o1) (inflate o2)

-- | Allocates values from the first list to the buckets in the second list.
allocate :: (a -> a -> a) -> [(Int, a)] -> [(Int, a)] -> [a]
allocate _ _ [] = []
allocate _ [] ys = map snd ys
allocate f (x:xs) (y:ys) = allocate f xs (L.sortBy (compare `on` fst) ((combine x y):ys))
  where
  combine (s1, v1) (s2, v2) = (s1 + s2, f v1 v2)

-- | Create empty buckets for allocating indexes.  
createBuckets :: Int -> [(Int, InvIndex)]
createBuckets n = (replicate n (0, emptyInverted))
  
-- | Convert the differences back to a set of integers.
inflate :: IntMap DiffList -> Occurrences
inflate = IM.map DL.toIntSet

-- | Save some memory on the positions by just saving their differences.
deflate :: Occurrences -> IntMap DiffList
deflate = IM.map DL.fromIntSet

-- | Create an empty index.
emptyInverted :: InvIndex
emptyInverted = InvIndex M.empty
                  
-- | Return a part of the index for a given context.
getPart :: Context -> InvIndex -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | The XML pickler for the index parts.
xpParts :: PU Parts
xpParts = xpWrap (M.fromList, M.toList) (xpList xpContext)
  where
  xpContext = xpElem "part" (xpPair (xpAttr "id" xpText) xpPart)

-- | The XML pickler for a single part.
xpPart :: PU Part
xpPart = xpElem "index" (xpWrap (SM.fromList, SM.toList) (xpList xpWord))
  where
  xpWord = xpElem "word" (xpPair (xpAttr "w" xpText) (xpWrap (deflate, inflate) xpOccurrences))
