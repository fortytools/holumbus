{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.IndexerCore
where

-- ------------------------------------------------------------

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State
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

type RawDoc u			= (RawContexts, RawTitle, Maybe u)		-- u is the user defined custom info
type RawContexts		= [RawContext]
type RawContext                 = (Context, RawWords)
type RawWords                   = [RawWord]
type RawWord			= (Word, Position)
type RawTitle			= String

type IndexCrawlerConfig i u	= CrawlerConfig (RawDoc u) (IndexerState i u)

data IndexContextConfig		= IndexContextConfig
    				  { ixc_name           :: String
				  , ixc_collectText    :: IOSArrow XmlTree String
				  , ixc_textToWords    :: String -> [String]
				  , ixc_boringWord     :: String -> Bool
				  }   

data (HolIndex i) =>
     IndexerState i u		= IndexerState
				  { ixs_index		:: i			-- the index type
				  , ixs_documents	:: Documents u		-- the user defined type for document descriptions
				  }

-- ------------------------------------------------------------

instance (HolIndex i, Binary u) => Binary (IndexerState i u) where
    put	s		= B.put (ixs_index s)
			  >>
			  B.put (ixs_documents s)
    get			= do
			  ix <- B.get
			  dm <- B.get
			  return $
				 IndexerState
				 { ixs_index		= ix
				 , ixs_documents	= dm
				 }

-- ------------------------------------------------------------

indexCrawlerConfig		:: Attributes					-- ^ document read options
				-> (URI -> Bool)				-- ^ the filter for deciding, whether the URI shall be processed
				-> Maybe (IOSArrow XmlTree String)		-- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
				-> Maybe (IOSArrow XmlTree XmlTree)		-- ^ the pre document filter, default is the this arrow
				-> Maybe (IOSArrow XmlTree String)		-- ^ the filter for computing the document title, default is empty string
				-> Maybe (IOSArrow XmlTree c)			-- ^ the filter for the cutomized doc info, default Nothing
				-> [IndexContextConfig]				-- ^ the configuration of the various index parts
				-> IndexCrawlerConfig i c			-- ^ result is a crawler config

indexCrawlerConfig opts	followRef getHrefF preDocF titleF0 customF0 contextCs
				= addReadAttributes defaultOpts			-- install the default read options
				  >>>
				  addReadAttributes opts			-- overwrite and add specific read options
				  >>>
				  ( setS theFollowRef followRef )
				  >>>
				  ( setS theProcessRefs   $ fromMaybe getHtmlReferences getHrefF )
				  >>>
				  ( setS thePreDocFilter  $ fromMaybe checkDocumentStatus preDocF )	-- in case of errors throw away any contents
				  >>>
				  ( setS theProcessDoc rawDocF )		-- rawDocF is build up by the context config, text, title and custom
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
    rawDocF			= ( listA contextFs
				    &&&
				    titleF
				    &&&
				    customF
				  )
                                  >>^ (\ (x3, (x2, x1)) -> (x3, x2, x1))

    titleF			= ( fromMaybe (constA "") titleF0 ) >. concat

    customF			= ( fromMaybe none customF0 ) >. listToMaybe
				  
    contextFs			:: IOSArrow XmlTree RawContext
    contextFs			= catA . map contextF $ contextCs		-- collect all contexts

    contextF			:: IndexContextConfig -> IOSArrow XmlTree RawContext
    contextF ixc		= constA (ixc_name ixc)				-- the name of the raw context
				  &&&
				  ( ixc_collectText ixc >. processText )	-- the list of words and positions of the collected text
	where									-- this arrow is deterministic, it always delivers a single pair
	processText		:: [String] -> RawWords
	processText		= concat
				  >>>
				  ixc_textToWords ixc
				  >>>
				  flip zip [1..]
				  >>>
				  filter (fst >>> ixc_boringWord ixc >>> not)

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


emptyIndexerState		:: (HolIndex i) => i -> IndexerState i u
emptyIndexerState eix		= IndexerState
				  { ixs_index		= eix
				  , ixs_documents	= emptyDocuments
				  }

-- ------------------------------------------------------------

insertRawDoc			:: (URI, RawDoc u) -> IndexerState i u -> IO (IndexerState i u)
insertRawDoc (_uri, (_rcs, _rtlt, _u)) _ixs
				= return undefined

-- ------------------------------------------------------------

stdIndexer			:: (HolIndex i, Binary c) =>
				   IndexCrawlerConfig i c					-- ^ adapt configuration to special needs, use id if default is ok
				-> i								-- ^ the initial empty indexer state
				-> Maybe String							-- ^ resume from interrupted index run with state stored in file
				-> [URI]							-- ^ start indexing with this set of uris
				-> IO (IndexerState i c)					-- ^ result is a state consisting of the index and the map of indexed documents

stdIndexer config eix resumeLoc startUris
				= do
				  (_, ixState) <- runCrawler action config (initCrawlerState $ emptyIndexerState eix)
				  return (getS theResultAccu ixState)
    where
    action			= maybe (crawlDocs startUris) crawlerResume $ resumeLoc

-- ------------------------------------------------------------

