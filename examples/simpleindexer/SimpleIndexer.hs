{-# OPTIONS #-}

-- ------------------------------------------------------------

module SimpleIndexer
where

import		 Holumbus.Crawler.Constants	( )
import		 Holumbus.Crawler.IndexerCore
import		 Holumbus.Crawler.Html
import		 Holumbus.Crawler.URIs
import		 Holumbus.Crawler.Util

import		 Holumbus.Index.Inverted.Memory

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

type PlainText			= String

type SimpleIndexerState		= IndexerState       Inverted PlainText
type SimpleIndexerConfig	= IndexCrawlerConfig Inverted PlainText

simpleIndexerConfig		:: (URI -> Bool) -> SimpleIndexerConfig
simpleIndexerConfig followRef	= indexCrawlerConfig
				  [ ]							-- use default read options
				  followRef						-- the set of URIs to be followed and processed 
				  Nothing						-- use default collection filter
				  Nothing						-- use the pre hrefs filter as  pre document filter
				  (Just getHtmlTitle)					-- the document title
				  (Just $ getHtmlPlainText >>^ limitLength 128)		-- the customized doc info: the first 128 chars of the the plain text
				  []							-- the context configs

simpleIndexer 			:: (URI -> Bool)					-- uris to be processed
                                -> [URI]						-- start uris
                                -> IO (IndexerState Inverted PlainText)			-- the index and document table with start of plain text
simpleIndexer refs startUris	= stdIndexer
				  ( simpleIndexerConfig refs )
				  emptyInverted
				  Nothing
				  startUris

-- ------------------------------------------------------------

siIndexer 			:: IO (IndexerState Inverted PlainText)
siIndexer			= simpleIndexer refs startUris 
    where
    startUris			= [ "http://localhost/~si/" ]
    refs			= simpleFollowRef'
				  [ "http://localhost/~si/termine/.*"			-- just 2 subdirs
				  , "http://localhost/~si/Klausuren/.*"
				  ]
				  [ ".*[?].*"						-- no query string
				  , "http://localhost/~si/vorlesungen/.*"		-- no lecture pages, currently redundant
				  ]
				  

-- ------------------------------------------------------------
