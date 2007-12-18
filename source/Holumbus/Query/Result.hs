-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Result
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The data type for results of Holumbus queries.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Result 
  (
  -- * Result data types
  Result (Result, docHits, wordHits)
  , DocHits
  , DocContextHits
  , DocWordHits
  , WordHits
  , WordContextHits
  , WordDocHits
  
  -- * Construction
  , emptyResult
  , emptyDocHits
  , emptyWordHits
  , fromList  
  , createDocHits
  , createWordHits

  -- * Combine
  , union
  , difference
  , intersection
  )
where

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

import Holumbus.Index.Common

-- | The combined result type for Holumbus queries.
data Result = Result { docHits  :: !DocHits
                     , wordHits :: !WordHits
                     } deriving (Show)

type DocHits = IntMap DocContextHits           -- Key is document id
type DocContextHits = Map Context DocWordHits
type DocWordHits = Map String Positions

type WordHits = Map String WordContextHits
type WordContextHits = Map Context WordDocHits
type WordDocHits = IntMap Positions            -- Key is document id

-- | Create an empty result.
emptyResult :: Result
emptyResult = Result IM.empty M.empty

emptyDocHits :: DocHits
emptyDocHits = IM.empty

emptyWordHits :: WordHits
emptyWordHits = M.empty

-- | Create the document hits structure for the results from a single context.
createDocHits :: Context -> [(String, Occurrences)] -> DocHits
createDocHits c = foldr (\(s, o) r -> IM.foldWithKey (insertDocHit c s) r o) emptyDocHits

-- | Inserts the positions of a word in a document from a context into the result.
insertDocHit :: Context -> String -> Int -> Positions -> DocHits -> DocHits
insertDocHit c w d p r = IM.insert d (M.insert c (M.insertWith IS.union w p dwh) dch) r
  where
  dch = IM.findWithDefault M.empty d r
  dwh = M.findWithDefault M.empty c dch

-- | Create the word hits structure for the results from a single context.
createWordHits :: Context -> [(String, Occurrences)] -> WordHits
createWordHits c = foldr (\(s, o) r -> IM.foldWithKey (insertWordHit c s) r o) emptyWordHits

-- | Inserts the positions of a word in a document from a context into the result.
insertWordHit :: Context -> String -> Int -> Positions -> WordHits -> WordHits
insertWordHit c w d p r = M.insert w (M.insert c (IM.insertWith IS.union d p wdh) wch) r
  where
  wch = M.findWithDefault M.empty w r
  wdh = M.findWithDefault IM.empty c wch

-- | Combine two results by calculating their union.
union :: Result -> Result -> Result
union (Result d1 w1) (Result d2 w2) = Result (unionDocHits d1 d2) (unionWordHits w1 w2)
  where
  unionDocHits = IM.unionWith (M.unionWith (M.unionWith IS.union))

-- | Combine two results by calculating their intersection.
intersection :: Result -> Result -> Result
intersection (Result d1 w1) (Result d2 w2) = Result (intersectDocHits d1 d2) (unionWordHits w1 w2)
  where
  intersectDocHits = IM.intersectionWith (M.unionWith (M.unionWith IS.union))

-- | Combine two results by calculating their difference.
difference :: Result -> Result -> Result
difference (Result d1 _) (Result d2 w2) = Result (IM.difference d1 d2) w2

unionWordHits :: WordHits -> WordHits -> WordHits
unionWordHits = M.unionWith (M.unionWith (IM.unionWith IS.union))

-- | Create a result from a list of tuples.
fromList :: Context -> [(String, Occurrences)] -> Result
fromList c xs = Result (createDocHits c xs) (createWordHits c xs)
