{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.IndexerCore
where

-- ------------------------------------------------------------

import		 Holumbus.Crawler.Core
import		 Holumbus.Crawler.URIs

import		 Holumbus.Index.Common		hiding ( URI )
import		 Holumbus.Index.Documents

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

type RawDoc u			= (RawContexts, RawText, Maybe u)		-- u is the user defined custom info
type RawContexts		= [RawContext]
type RawContext                 = (Context, RawWords)
type RawWords                   = [RawWord]
type RawWord			= (Word, Position)
type RawText			= String

type IxCrawlerConfig i u	= CrawlerConfig (RawDoc u) (IndexerState i u)

data IndexerConfig i u		= IndexerConfig
				  { icc_crawlerConfig	:: IxCrawlerConfig i u
				  , icc_docTitle	:: IOSArrow XmlTree String
				  , icc_rawText		:: IOSArrow XmlTree String
				  , icc_custom		:: IOSArrow XmlTree u
				  , icc_contexts	:: [ContextConfig]
				  }

data ContextConfig		= ContextConfig
				  { }

data (HolIndex i) =>
     IndexerState i u		= IndexerState
				  { ixs_index		:: i			-- the index type
				  , ixs_documents	:: Documents u		-- the user defined type for document descriptions
				  }

-- ------------------------------------------------------------

defaultIndexerConfig		:: IndexerConfig i u
defaultIndexerConfig		= IndexerConfig
				  { icc_crawlerConfig	= defaultCrawlerConfig insertRawDoc
				  , icc_docTitle	= none
				  , icc_rawText		= none
				  , icc_custom		= none
				  , icc_contexts	= []
				  }

defaultIndexerState		:: (HolIndex i) => i -> IndexerState i u
defaultIndexerState ix		= IndexerState
				  { ixs_index		= ix
				  , ixs_documents	= emptyDocuments
				  }

-- ------------------------------------------------------------

insertRawDoc			:: (URI, RawDoc u) -> IndexerState i u -> IO (IndexerState i u)
insertRawDoc (_uri, (_rcs, _rt, _u)) _ixs
				= return undefined

-- ------------------------------------------------------------

