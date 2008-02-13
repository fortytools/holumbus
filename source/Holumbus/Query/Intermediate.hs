-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Intermediate
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The data type for intermediate results occuring during query processing.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Intermediate 
(
  -- * The intermediate result type.
  Intermediate 

  -- * Construction
  , emptyIntermediate

  -- * Query
  , null
  , sizeIntermediate

  -- * Combine
  , union
  , difference
  , intersection  
  , unions
  
  -- * Conversion
  , fromList
  , toResult
)
where

import Prelude hiding (null)

import Data.Maybe

import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

import Holumbus.Query.Result hiding (null, merge)

import Holumbus.Index.Common

-- | The intermediate result used during query processing.
type Intermediate = IntMap IntermediateContexts
type IntermediateContexts = Map Context IntermediateWords
type IntermediateWords = Map Word (WordInfo, Positions)

-- | Create an empty intermediate result.
emptyIntermediate :: Intermediate
emptyIntermediate = IM.empty

-- | Check if the intermediate result is empty.
null :: Intermediate -> Bool
null = IM.null

-- | Returns the number of documents in the intermediate result.
sizeIntermediate :: Intermediate -> Int
sizeIntermediate = IM.size

-- | Merges a bunch of intermediate results into one intermediate result by unioning them.
unions :: [Intermediate] -> Intermediate
unions = L.foldl' union emptyIntermediate

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
fromList :: Word -> Context -> RawResult -> Intermediate
-- Beware! This is extremly optimized and will not work for merging arbitrary intermediate results!
-- Based on resultByDocument from Holumbus.Index.Common
fromList t c os = IM.map transform $ IM.unionsWith (flip $ (:) . head) (map insertWords os)
  where
  insertWords (w, o) = IM.map (\p -> [(w, (WordInfo [t] 0.0 , p))]) o   
  transform w = M.singleton c (M.fromList w)

-- | Convert to a @Result@ by generating the 'WordHits' structure.
toResult :: HolDocuments d => d -> Intermediate -> Result
toResult d im = Result (createDocHits d im) (createWordHits im)

-- | Create the doc hits structure from an intermediate result.
createDocHits :: HolDocuments d => d -> Intermediate -> DocHits
createDocHits d im = IM.mapWithKey transformDocs im
  where
  transformDocs did ic = let doc = fromMaybe ("", "") (lookupById d did) in
                       (DocInfo doc 0.0, M.map (M.map (\(_, p) -> p)) ic)

-- | Create the word hits structure from an intermediate result.
createWordHits :: Intermediate -> WordHits
createWordHits im = IM.foldWithKey transformDoc M.empty im
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
