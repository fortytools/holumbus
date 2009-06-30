{-# OPTIONS #-}

-- ------------------------------------------------------------

module SimpleIndexer
where

import		 Holumbus.Crawler.Constants	( )
import		 Holumbus.Crawler.IndexerCore
import		 Holumbus.Crawler.Html

import		 Holumbus.Index.Inverted.Memory

import		 Text.XML.HXT.Arrow		( )

-- ------------------------------------------------------------

type PlainText			= String

type SimpleIndexerState		= IndexerState       Inverted PlainText
type SimpleIndexerConfig	= IndexCrawlerConfig Inverted PlainText

simpleIndexerState		:: SimpleIndexerState
simpleIndexerState		= defaultIndexerState emptyInverted

simpleIndexerConfig		:: SimpleIndexerConfig
simpleIndexerConfig		= indexCrawlerConfig
				   [ ]							-- use default read options
				  Nothing						-- use default collection filter
				  Nothing						-- use the default pre document filter: this
				  (Just getHtmlTitle)					-- the document title
				  Nothing						-- the raw text
				  Nothing						-- the customized doc info
				  []							-- the context configs

-- ------------------------------------------------------------
