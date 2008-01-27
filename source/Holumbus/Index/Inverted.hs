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
  , emptyInverted
)
where

import Text.XML.HXT.Arrow

import Data.Maybe
import Data.Binary

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

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
  lookupCase i c q = map inflate $ maybeToList (SM.lookup q $ getPart c i)
  lookupNoCase i c q = map inflate $ SM.lookupNoCase q $ getPart c i

  mergeIndexes _ _ = emptyInverted

  insertOccurrences _ _ _ _ = emptyInverted

instance NFData InvIndex where
  rnf (InvIndex parts) = rnf parts

instance XmlPickler InvIndex where
  xpickle =  xpElem "indexes" $ xpWrap (\p -> InvIndex p, \(InvIndex p) -> p) xpParts

instance Binary InvIndex where
  put (InvIndex parts) = do
                              put parts
  get = do
        parts <- get
        return (InvIndex parts)

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
