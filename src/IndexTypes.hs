{-# OPTIONS #-}

-- ------------------------------------------------------------

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
import qualified Data.IntMap                    as IM
import           Data.Maybe

import           PageInfo

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import           Holumbus.Index.Common          ( Document(..)
                                                , Occurrences
                                                , toList, fromList
                                                , editDocIds, toMap, updateDocIds'
                                                )
import           Holumbus.Index.CompactDocuments
                                                ( Documents(..)
                                                , emptyDocuments
                                                )

import           Holumbus.Index.CompactSmallDocuments
                                                ( SmallDocuments(..)
                                                , docTable2smallDocTable
                                                )

import           Holumbus.Query.Result          ( Score )

-- import           Debug.Trace

-- ------------------------------------------------------------
 
import qualified Holumbus.Index.Inverted.CompressedPrefixMem    as PM

type Inverted                   = PM.Inverted0

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInverted0

removeDocIdsInverted            :: Occurrences -> Inverted -> Inverted
removeDocIdsInverted            = PM.removeDocIdsInverted

type CompactInverted            = PM.InvertedOSerialized

inverted2compactInverted        :: Inverted -> CompactInverted
inverted2compactInverted        = fromList PM.emptyInvertedOSerialized . toList

-- ------------------------------------------------------------

type W3WState  di               = IndexerState       Inverted Documents di
type W3WConfig di               = IndexCrawlerConfig Inverted Documents di

type W3WrCrawlerState di        = CrawlerState (W3WState di)

emptyW3WState                   :: W3WState di
emptyW3WState                   = emptyIndexerState emptyInverted emptyDocuments

-- ------------------------------------------------------------

defragmentIndex                 :: (Binary di) =>
                                   W3WState di -> W3WState di
defragmentIndex IndexerState
              { ixs_index     = ix
              , ixs_documents = ds
              }                 = IndexerState
                                  { ixs_index     = ix'
                                  , ixs_documents = ds'
                                  }
    where
    ix'                         = updateDocIds' editId ix
    ds'                         = editDocIds editId ds
    idMap                       = IM.fromList . flip zip [1..] . IM.keys . toMap $ ds
    editId i                    = fromJust . IM.lookup i $ idMap

-- ------------------------------------------------------------

type W3WIndexerState                  = W3WState   PageInfo
type W3WIndexerConfig                 = W3WConfig  PageInfo

type W3WIndexerCrawlerState           = CrawlerState W3WIndexerState

-- ------------------------------------------------------------

