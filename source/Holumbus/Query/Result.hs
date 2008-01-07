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
  Result (..)
  , DocHits
  , DocContextHits
  , DocWordHits
  , WordHits
  , WordContextHits
  , WordDocHits
  , Score
  
  -- * Construction
  , emptyResult
  , emptyDocHits
  , emptyWordHits
  , fromList  
  , createDocHits
  , createWordHits

  -- * Query
  , size
  , null

  -- * Combine
  , union
  , difference
  , intersection
  
  -- * Pickling
  , xpResult
  , xpDocHits
  , xpWordHits
  )
where

import Prelude hiding (null)

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.IntSet as IS

import Text.XML.HXT.Arrow.Pickle

import Holumbus.Index.Common

-- | The combined result type for Holumbus queries.
data Result = Result { docHits  :: !DocHits
                     , wordHits :: !WordHits
                     } deriving (Show)

type DocHits = IntMap (Score, DocContextHits)         -- Key is document id, fst is score
type DocContextHits = Map Context DocWordHits
type DocWordHits = Map String Positions               -- Key is word

type WordHits = Map String (Score, WordContextHits)   -- Key is word, fst is score
type WordContextHits = Map Context WordDocHits
type WordDocHits = Occurrences                        -- Key is document id

type Score = Float

instance XmlPickler Result where
  xpickle = xpWrap (\(d, w) -> Result d w, \(Result d w) -> (d, w)) (xpPair xpDocHits xpWordHits)

-- | The XML pickler for the result type.
xpResult :: PU Result
xpResult = xpElem "result" $ xpickle

xpDocHits :: PU DocHits
xpDocHits = xpElem "dochits" $ xpWrap (IM.fromList, IM.toList) (xpList xpScoredDoc)
  where
  xpScoredDoc = xpElem "doc" (xpPair (xpAttr "idref" xpPrim) (xpPair (xpAttr "score" xpPrim) xpDocContextHits))

xpDocContextHits :: PU DocContextHits
xpDocContextHits = xpWrap (M.fromList, M.toList) (xpList xpDocContextHit)
  where
  xpDocContextHit = xpElem "context" (xpPair (xpAttr "name" xpText) xpDocWordHits)

xpDocWordHits :: PU DocWordHits
xpDocWordHits = xpWrap (M.fromList, M.toList) (xpList xpDocWordHit)
  where
  xpDocWordHit = xpElem "word" (xpPair (xpAttr "w" xpText) xpPositions)

xpWordHits :: PU WordHits
xpWordHits = xpElem "wordhits" $ xpWrap (M.fromList, M.toList) (xpList xpScoredWord)
  where
  xpScoredWord = xpElem "word" (xpPair (xpAttr "w" xpText) (xpPair (xpAttr "score" xpPrim) xpWordContextHits))

xpWordContextHits :: PU WordContextHits
xpWordContextHits = xpWrap (M.fromList, M.toList) (xpList xpWordContextHit)
  where
  xpWordContextHit = xpElem "context" (xpPair (xpAttr "name" xpText) xpWordDocHits)

xpWordDocHits :: PU WordDocHits
xpWordDocHits = xpOccurrences

-- | Create an empty result.
emptyResult :: Result
emptyResult = Result IM.empty M.empty

-- | Create an empty set of document hits.
emptyDocHits :: DocHits
emptyDocHits = IM.empty

-- | Create an empty set of word hits.
emptyWordHits :: WordHits
emptyWordHits = M.empty

-- | Query the size of a result.
size :: Result -> Int
size = IM.size . docHits

-- | Test if the result contains anything.
null :: Result -> Bool
null = IM.null . docHits

-- | Create the document hits structure for the results from a single context.
createDocHits :: Context -> [(String, Occurrences)] -> DocHits
createDocHits c = foldr (\(s, o) r -> IM.foldWithKey (insertDocHit c s) r o) emptyDocHits

-- | Inserts the positions of a word in a document from a context into the result.
insertDocHit :: Context -> String -> Int -> Positions -> DocHits -> DocHits
insertDocHit c w d p r = IM.insert d (0.0, (M.insert c (M.insertWith IS.union w p dwh) dch)) r
  where
  dch = snd $ IM.findWithDefault (0.0, M.empty) d r
  dwh = M.findWithDefault M.empty c dch

-- | Create the word hits structure for the results from a single context.
createWordHits :: Context -> [(String, Occurrences)] -> WordHits
createWordHits c = foldr (\(s, o) r -> IM.foldWithKey (insertWordHit c s) r o) emptyWordHits

-- | Inserts the positions of a word in a document from a context into the result.
insertWordHit :: Context -> String -> Int -> Positions -> WordHits -> WordHits
insertWordHit c w d p r = M.insert w (0.0, (M.insert c (IM.insertWith IS.union d p wdh) wch)) r
  where
  wch = snd $ M.findWithDefault (0.0, M.empty) w r
  wdh = M.findWithDefault IM.empty c wch

-- | Combine two results by calculating their union.
union :: Result -> Result -> Result
union (Result d1 w1) (Result d2 w2) = Result (unionDocHits d1 d2) (unionWordHits w1 w2)

-- | Combine two results by calculating their intersection.
intersection :: Result -> Result -> Result
intersection (Result d1 w1) (Result d2 w2) = Result (intersectDocHits d1 d2) (unionWordHits w1 w2)

-- | Combine two results by calculating their difference.
difference :: Result -> Result -> Result
difference (Result d1 _) (Result d2 w2) = Result (IM.difference d1 d2) w2

-- | Intersect two sets of document hits.
intersectDocHits :: DocHits -> DocHits -> DocHits
intersectDocHits = IM.intersectionWith combineDocHits

-- | Combine two sets of document hits.
unionDocHits :: DocHits -> DocHits -> DocHits
unionDocHits = IM.unionWith combineDocHits

-- | Combine two sets of word hits.
unionWordHits :: WordHits -> WordHits -> WordHits
unionWordHits = M.unionWith combineWordHits

-- | Combine two tuples with score and context hits.
combineDocHits :: (Score, DocContextHits) -> (Score, DocContextHits) -> (Score, DocContextHits)
combineDocHits (s1, c1) (s2, c2) = (unionScore s1 s2, M.unionWith (M.unionWith IS.union) c1 c2)

-- | Combine two tuples with score and context hits.
combineWordHits :: (Score, WordContextHits) -> (Score, WordContextHits) -> (Score, WordContextHits)
combineWordHits (s1, c1) (s2, c2) = (unionScore s1 s2, M.unionWith (IM.unionWith IS.union) c1 c2)

-- | Combine two scores (just average between them).
unionScore :: Score -> Score -> Score
unionScore = flip flip 2.0 . ((/) .) . (+)

-- | Create a result from a list of tuples.
fromList :: Context -> [(String, Occurrences)] -> Result
fromList c xs = Result (createDocHits c xs) (createWordHits c xs)
