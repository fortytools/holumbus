-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Ranking
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  The ranking mechanism for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Ranking 
  (
  -- * Ranking types
  RankConfig (..)
  , DocRanking
  , WordRanking
  
  -- * Ranking
  , rank
  
  -- * Predefined document rankings
  , docRankByCount
  , docRankWeightedByCount
  
  -- * Predefined word rankings
  , wordRankByCount
  , wordRankWeightedByCount
  )
where

import Prelude hiding (foldr)

import Data.Function
import Data.Foldable

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Holumbus.Query.Result
import Holumbus.Index.Common

-- | The configuration of the ranking mechanism.
data RankConfig = RankConfig 
  { docRanking :: DocRanking   -- ^ A function to determine the score of a document.
  , wordRanking :: WordRanking -- ^ A funciton to determine the score of a word.
  }

-- | The signature of a function to determine the score of a document.
type DocRanking = DocId -> DocContextHits -> Score
-- | The signature of a function to determine the score of a word.
type WordRanking = Word -> WordContextHits -> Score

-- | Rank the result with custom ranking functions.
rank :: RankConfig -> Result -> Result
rank (RankConfig fd fw) r = Result scoredDocHits scoredWordHits
  where
  scoredDocHits = IM.mapWithKey (\k (di, dch) -> (setDocScore (fd k dch) di, dch)) $ docHits r
  scoredWordHits = M.mapWithKey (\k (wi, wch) -> (setWordScore (fw k wch) wi, wch)) $ wordHits r

-- | Rank documents by count.
docRankByCount :: DocId -> DocContextHits -> Score
docRankByCount _ h = fromIntegral $ M.fold (\h1 r1 -> M.fold (\h2 r2 -> IS.size h2 + r2) r1 h1) 0 h

-- | Rank words by count.
wordRankByCount :: Word -> WordContextHits -> Score
wordRankByCount _ h = fromIntegral $ M.fold (\h1 r1 -> IM.fold ((+) . IS.size) r1 h1) 0 h

-- | Rank documents by context-weighted count. The weights will be normalized to a maximum of 1.0.
-- Contexts with no weight (or a weight of zero) will be ignored.
docRankWeightedByCount :: [(Context, Score)] -> DocId -> DocContextHits -> Score
docRankWeightedByCount ws _ h =  M.foldWithKey (calcWeightedScore ws) 0.0 h

-- | Rank words by context-weighted count. The weights will be normalized to a maximum of 1.0.
-- Contexts with no weight (or a weight of zero) will be ignored.
wordRankWeightedByCount :: [(Context, Score)] -> Word -> WordContextHits -> Score
wordRankWeightedByCount ws _ h = M.foldWithKey (calcWeightedScore ws) 0.0 h

-- | Calculate the weighted score of occurrences of a word.
calcWeightedScore :: (Foldable f) => [(Context, Score)] -> Context -> (f IS.IntSet) -> Score -> Score
calcWeightedScore ws c h r = maybe r (\w -> r + ((w / mw) * count)) (lookupWeight c ws)
  where
  count = fromIntegral $ foldl' (flip $ (+) . IS.size) 0 h
  mw = snd $ L.maximumBy (compare `on` snd) ws

-- | Find the weight of a context in a list of weights. If the context was not found or it's
-- weight is equal to zero, 'Nothing' will be returned.
lookupWeight :: Context -> [(Context, Score)] -> Maybe Score
lookupWeight _ [] = Nothing
lookupWeight c (x:xs) = if fst x == c then
                          if snd x /= 0.0
                          then Just (snd x)
                          else Nothing
                        else lookupWeight c xs
