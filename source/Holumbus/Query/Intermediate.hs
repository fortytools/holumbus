-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Intermediate
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The data type for intermediate results occuring during query processing.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Intermediate 
(
  -- * The intermediate result type.
  Intermediate 

  -- * Construction
  , empty

  -- * Query
  , null

  -- * Combine
  , union
  , difference
  , intersection  
  
  -- * Conversion
  , fromList
  , toResult
)
where

import Prelude hiding (null)

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

import qualified Holumbus.Index.Documents as D

import Holumbus.Query.Result hiding (null, merge)

import Holumbus.Index.Common

-- | The intermediate result used during query processing.
type Intermediate = IntMap IntermediateContexts
type IntermediateContexts = Map Context IntermediateWords
type IntermediateWords = Map Word (WordInfo, Positions)

-- | Create an empty intermediate result.
empty :: Intermediate
empty = IM.empty

-- | Check if the intermediate result is empty.
null :: Intermediate -> Bool
null = IM.null

-- | Intersect two sets of intermediate results.
intersection :: Intermediate -> Intermediate -> Intermediate
intersection = IM.intersectionWith combineContexts

-- | Union two sets of intermediate results.
union :: Intermediate -> Intermediate -> Intermediate
union = IM.unionWith combineContexts

-- | Substract two sets of intermediate results.
difference :: Intermediate -> Intermediate -> Intermediate
difference = IM.difference

-- | Create an intermediate result from a list of words and their occurrences.
fromList :: Word -> Context -> [(String, Occurrences)] -> Intermediate
fromList t c os = IM.unionsWith combineContexts (map createIntermediate' os)
  where
  createIntermediate' (w, o) = IM.map (\p -> M.singleton c (M.singleton w (WordInfo [t] 0.0, p))) o

-- | Convert to a @Result@ by generating the @WordHits@ structure.
toResult :: HolIndex i => i -> Intermediate -> Result
toResult i im = Result (createDocHits i im) (createWordHits i im)

-- | Create the doc hits structure from an intermediate result.
createDocHits :: HolIndex i => i -> Intermediate -> DocHits
createDocHits i im = IM.mapWithKey transformDocs im
  where
  transformDocs d ic = let doc = fromMaybe ("", "") (D.lookupId d (documents i)) in
                       (DocInfo doc 0.0, M.map (M.map (\(_, p) -> p)) ic)

-- | Create the word hits structure from an intermediate result.
createWordHits :: HolIndex i => i -> Intermediate -> WordHits
createWordHits _ im = IM.foldWithKey transformDoc M.empty im
  where
  transformDoc d ic wh = M.foldWithKey transformContext wh ic
    where
    transformContext c iw wh' = M.foldWithKey insertWord wh' iw
      where
      insertWord w (wi, pos) wh'' = M.insertWith combineWordHits w (wi, M.singleton c (IM.singleton d pos)) wh''

-- | Combine two tuples with score and context hits.
combineWordHits :: (WordInfo, WordContextHits) -> (WordInfo, WordContextHits) -> (WordInfo, WordContextHits)
combineWordHits (i1, c1) (i2, c2) = (combineWordInfo i1 i2, M.unionWith (IM.unionWith IS.union) c1 c2)

-- | Combine two tuples with score and context hits.
combineContexts :: IntermediateContexts -> IntermediateContexts -> IntermediateContexts
combineContexts = M.unionWith (M.unionWith merge)
  where
  merge (i1, p1) (i2, p2) = (combineWordInfo i1 i2, IS.union p1 p2)

-- | Combine two word informations.
combineWordInfo :: WordInfo -> WordInfo -> WordInfo
combineWordInfo (WordInfo t1 s1) (WordInfo t2 s2) = WordInfo (t1 ++ t2) (combineScore s1 s2)

-- | Combine two scores (just average between them).
combineScore :: Score -> Score -> Score
combineScore = flip flip 2.0 . ((/) .) . (+)
