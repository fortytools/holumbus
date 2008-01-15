-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Ranking
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The ranking mechanism for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Ranking 
  (
  -- * Ranking
  rank
  , rankWith
  
  -- * Predefined document rankings
  , docRankByCount
  
  -- * Predefined word rankings
  , wordRankByCount
  )
where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Holumbus.Query.Result
import Holumbus.Index.Documents
import Holumbus.Index.Common

type DocRanking = DocId -> DocContextHits -> Score
type WordRanking = Word -> WordContextHits -> Score

-- | The default ranking for document hits.
docDefaultRanking :: DocId -> DocContextHits -> Score
docDefaultRanking = docRankByCount

-- | The default ranking for word hits.
wordDefaultRanking :: Word -> WordContextHits -> Score
wordDefaultRanking = wordRankByCount

-- | Rank with the default ranking mechanism.
rank :: Result -> Result
rank = rankWith docDefaultRanking wordDefaultRanking

-- | Rank the result with custom ranking functions.
rankWith :: DocRanking -> WordRanking -> Result -> Result
rankWith fd fw r = Result scoredDocHits scoredWordHits
  where
  scoredDocHits = IM.mapWithKey (\k (di, dch) -> (setDocScore (fd k dch) di, dch)) $ docHits r
  scoredWordHits = M.mapWithKey (\k (wi, wch) -> (setWordScore (fw k wch) wi, wch)) $ wordHits r

-- | Rank documents by count.
docRankByCount :: DocId -> DocContextHits -> Score
docRankByCount _ h = fromIntegral $ M.fold (\h1 r1 -> M.fold (\h2 r2 -> IS.size h2 + r2) r1 h1) 0 h

-- | Rank words by count.
wordRankByCount :: Word -> WordContextHits -> Score
wordRankByCount _ h = fromIntegral $ M.fold (\h1 r1 -> IM.fold (\h2 r2 -> IS.size h2 + r2) r1 h1) 0 h
