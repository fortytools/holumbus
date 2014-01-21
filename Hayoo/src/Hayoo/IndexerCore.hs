{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ------------------------------------------------------------

module Hayoo.IndexerCore
    ( RawDoc
    , RawContexts
    , RawContext
    , RawWords
    , RawWord
    , RawTitle
    , IndexCrawlerConfig
    , IndexContextConfig(..)
    , IndexerState(..)
    , emptyIndexerState
    , indexCrawlerConfig
    , stdIndexer
    , unionIndexerStatesM
    )
where

-- ------------------------------------------------------------

import           Control.DeepSeq
import           Control.Monad                (foldM)

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Holumbus.Index.Common        hiding (URI)

import           Text.XML.HXT.Core

-- ------------------------------------------------------------
--
-- this code must be moved to an extra module, else indexerCore is to specialized,
-- JSON output can't be configurated with indexCrawlerConfig, the merge and inserts
-- have to be changed

indexCrawlerConfig             :: ( HolIndexM IO i
                                  , HolDocuments d c
                                  , HolDocIndex  d c i
                                  , NFData i
                                  , NFData c
                                  , NFData (d c)
                                  ) =>
                                   SysConfig                                    -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree c)                   -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> IndexCrawlerConfig i d c                     -- ^ result is a crawler config
indexCrawlerConfig = indexCrawlerConfig' insertRawDocM unionIndexerStatesM

unionIndexerStatesM             :: ( HolIndexM IO i
                                   , HolDocuments d c
                                   , HolDocIndex d c i
                                   ) =>
                                   IndexerState i d c
                                -> IndexerState i d c
                                -> IO (IndexerState i d c)
unionIndexerStatesM ixs1 ixs2
    = return
      $! IndexerState { ixs_index        = ix
                      , ixs_documents    = dt
                      }
    where
      (! dt, ! ix)              = unionDocIndex dt1 ix1 dt2 ix2
      ix1                       = ixs_index     ixs1
      ix2                       = ixs_index     ixs2
      dt1                       = ixs_documents ixs1
      dt2                       = ixs_documents ixs2

-- ------------------------------------------------------------

insertRawDocM                   :: ( HolIndexM IO i
                                   , HolDocuments d c
                                   , NFData i
                                   , NFData c
                                   , NFData (d c)
                                   ) =>
                                   (URI, RawDoc c)                              -- ^ extracted URI and doc info
                                -> IndexerState i d c                           -- ^ old indexer state
                                -> IO (IndexerState i d c)                      -- ^ new indexer state

insertRawDocM (rawUri, (rawContexts, rawTitle, rawCustom)) ixs
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise                 = do
                                  newIx  <- foldM (insertRawContextM did) (ixs_index ixs) $ rawContexts
                                  newIxs <- return $
                                            IndexerState { ixs_index        = newIx
                                                         , ixs_documents    = newDocs
                                                         }
                                  rnf newIxs `seq`
                                      return newIxs
    where
    nullContexts                = and . map (null . snd) $ rawContexts
    (did, newDocs)              = insertDoc (ixs_documents ixs) doc
    doc                         = Document
                                  { title       = rawTitle
                                  , uri         = rawUri
                                  , custom      = rawCustom
                                  }

insertRawContextM               :: (Monad m, HolIndexM m i) =>
                                   DocId -> i -> (Context, [(Word, Position)]) -> m i
insertRawContextM did ix (cx, ws)
                                = foldM (insWordM cx did) ix ws

insWordM                        :: (Monad m, HolIndexM m i) =>
                                   Context -> DocId -> i -> (Word, Position) -> m i
insWordM cx' did' ix' (w', p')  = insertPositionM cx' w' did' p' ix'

-- ------------------------------------------------------------
