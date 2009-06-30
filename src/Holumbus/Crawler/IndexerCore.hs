{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.IndexerCore
where

-- ------------------------------------------------------------

import           Data.Function.Selector
import		 Data.Maybe

import		 Holumbus.Crawler.Constants
import		 Holumbus.Crawler.Core
import		 Holumbus.Crawler.Html
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

type IndexCrawlerConfig i u	= CrawlerConfig (RawDoc u) (IndexerState i u)

data IndexerConfig i u		= IndexerConfig
				  { icc_crawlerConfig	:: IndexCrawlerConfig i u
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

indexCrawlerConfig		:: Attributes					-- ^ document read options
				-> Maybe (IOSArrow XmlTree String)		-- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
				-> Maybe (IOSArrow XmlTree XmlTree)		-- ^ the pre document filter, default is the this arrow
				-> Maybe (IOSArrow XmlTree String)		-- ^ the filter for computing the document title, default is empty string
				-> Maybe (IOSArrow XmlTree String)		-- ^ the filter for computing the raw text, default is empty string
				-> Maybe (IOSArrow XmlTree c)			-- ^ the filter for the cutomized doc info, default Nothing
				-> [ContextConfig]				-- ^ the configuration of the various index parts
				-> IndexCrawlerConfig i c			-- ^ result is a crawler config

indexCrawlerConfig opts	getHrefF preDocF titleF0 textF0 customF0 contextCs
				= addReadAttributes defaultOpts			-- install the default read options
				  >>>
				  addReadAttributes opts			-- overwrite and add specific read options
				  >>>
				  ( setS theProcessRefs  $ fromMaybe getHtmlReferences getHrefF )
				  >>>
				  ( setS thePreDocFilter $ fromMaybe checkDocumentStatus preDocF )	-- in case of errors throw away any contents
				  >>>
				  enableRobotsTxt				-- add the robots stuff at the end
				  >>>						-- the filter wrap the other filters
				  addRobotsNoFollow
				  >>>
				  addRobotsNoIndex
				  $
				  defaultCrawlerConfig insertRawDoc		-- take the default crawler config
										-- and set the result combining function
    where
    titleF			= fromMaybe (constA "") titleF0
    textF			= fromMaybe (constA "") textF0
    customF			= fromMaybe none        customF0

    contextFs			:: IOSArrow XmlTree RawContexts
    contextFs			= catA . map contextF $ contextCs

    contextF			:: ContextConfig -> IOSArrow XmlTree RawContexts
    contextF			= undefined

    defaultOpts			= [ (curl_max_filesize, 	"1000000")	-- limit document size to 1 Mbyte
				  , (curl_location, 		v_1)		-- automatically follow redirects
				  , (curl_max_redirects, 	"3")		-- but limit # of redirects to 3
				  , (a_accept_mimetypes, 	"text/html")
				  , (a_encoding,		isoLatin1)
				  , (a_ignore_encoding_errors, 	v_1)   		-- encoding errors and parser warnings are boring
				  , (a_validate,   		v_0)
				  , (a_parse_html,		v_1)
				  , (a_issue_warnings, 	v_0)
				  ]


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

