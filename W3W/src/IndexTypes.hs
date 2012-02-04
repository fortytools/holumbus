{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : IndexTypes

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable

  The List of sub indexes created in the index file
-}

-- ----------------------------------------------------------------------------

module IndexTypes
    ( module IndexTypes
    , module Holumbus.Index.CompactIndex
    , PageInfo(..)
    , Score
    )
where

import           PageInfo

import           Holumbus.Crawler
import           Holumbus.Index.CompactIndex
import           Holumbus.Query.Result          ( Score )

-- ------------------------------------------------------------

emptyW3WState :: W3WIndexerState
emptyW3WState
    = emptyIndexerState emptyInverted emptyDocuments

-- ------------------------------------------------------------

type W3WIndexerState        = HolumbusState   PageInfo
type W3WIndexerConfig       = HolumbusConfig  PageInfo
type W3WIndexerCrawlerState = CrawlerState W3WIndexerState

-- ------------------------------------------------------------

