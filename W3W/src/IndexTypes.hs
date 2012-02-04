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
    , Document
    , Documents
    , SmallDocuments
    , PageInfo(..)
    , Score
    , docTable2smallDocTable
    )
where

import           Data.Binary

import           PageInfo

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import           Holumbus.Index.Common          ( Document(..)
                                                , Occurrences
                                                , defragmentDocIndex
                                                , toList
                                                , fromList
                                                )

import           Holumbus.Index.CompactDocuments
                                                ( Documents(..)
                                                , emptyDocuments
                                                )

import           Holumbus.Index.CompactSmallDocuments
                                                ( SmallDocuments(..)
                                                , docTable2smallDocTable
                                                )

import qualified Holumbus.Index.CompactSmallDocuments
                                                as CSD

import           Holumbus.Query.Result          ( Score )


-- ------------------------------------------------------------

import qualified Holumbus.Index.Inverted.CompressedPrefixMem    as PM

type Inverted                   = PM.Inverted0

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInverted0

removeDocIdsInverted            :: Occurrences -> Inverted -> Inverted
removeDocIdsInverted            = PM.removeDocIdsInverted

type CompactInverted            = PM.InvertedOSerialized

emptyCompactInverted            :: CompactInverted
emptyCompactInverted            = PM.emptyInvertedOSerialized

inverted2compactInverted        :: Inverted -> CompactInverted
inverted2compactInverted        = fromList PM.emptyInvertedOSerialized . toList

-- ------------------------------------------------------------

type W3WState  di               = IndexerState       Inverted Documents di
type W3WConfig di               = IndexCrawlerConfig Inverted Documents di

type W3WrCrawlerState di        = CrawlerState (W3WState di)

emptyW3WState                   :: W3WState di
emptyW3WState                   = emptyIndexerState emptyInverted emptyDocuments

flushW3WState                 :: W3WState di -> W3WState di
flushW3WState hs              = hs { ixs_index     = emptyInverted
                                   , ixs_documents = emptyDocuments
                                                     { lastDocId = lastDocId . ixs_documents $ hs }
                                   }

emptySmallDocuments             :: SmallDocuments a
emptySmallDocuments             = CSD.emptyDocuments

-- ------------------------------------------------------------

defragmentIndex                 :: (Binary di) =>
                                   W3WState di -> W3WState di
defragmentIndex IndexerState
              { ixs_index     = ix
              , ixs_documents = dt
              }                 = IndexerState
                                  { ixs_index     = ix'
                                  , ixs_documents = dt'
                                  }
    where
      (dt', ix')                = defragmentDocIndex dt ix

-- ------------------------------------------------------------

type W3WIndexerState                  = W3WState   PageInfo

type W3WIndexerConfig                 = W3WConfig  PageInfo

type W3WIndexerCrawlerState           = CrawlerState W3WIndexerState

-- ------------------------------------------------------------

