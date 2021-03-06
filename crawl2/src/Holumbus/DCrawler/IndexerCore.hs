{-# OPTIONS -XFlexibleContexts #-} {- -}

-- ------------------------------------------------------------

module Holumbus.DCrawler.IndexerCore
where

-- ------------------------------------------------------------

import		 Control.Monad.Trans		( ) -- MonadIO )
import		 Control.DeepSeq

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State
import           Data.Function.Selector
import		 Data.List
import qualified Data.Map    			as M
import qualified Data.IntMap 			as IM
import qualified Data.IntSet 			as IS
import		 Data.Maybe

import		 Holumbus.DCrawler.Constants
import		 Holumbus.DCrawler.Core
import		 Holumbus.DCrawler.Html
import		 Holumbus.DCrawler.URIs

import		 Holumbus.Index.Common		hiding ( URI )
import           Holumbus.Index.Documents

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

type RawDoc c			= (RawContexts, RawTitle, Maybe c)		-- c is the user defined custom info
type RawContexts		= [RawContext]
type RawContext                 = (Context, RawWords)
type RawWords                   = [RawWord]
type RawWord			= (Word, Position)
type RawTitle			= String

type IndexCrawlerConfig i d c	= CrawlerConfig (RawDoc c) (IndexerState i d c)

data IndexContextConfig		= IndexContextConfig
    				  { ixc_name           :: String
				  , ixc_collectText    :: IOSArrow XmlTree String
				  , ixc_textToWords    :: String -> [String]
				  , ixc_boringWord     :: String -> Bool
				  }

data IndexerState i d c		= IndexerState
				  { ixs_index		:: ! i			-- the index type
				  , ixs_documents	:: ! (d c)		-- the user defined type for document descriptions
				  } deriving (Show)

instance (NFData i, NFData (d c)) => NFData (IndexerState i d c) where
  rnf IndexerState { ixs_index = i, ixs_documents = d } = rnf i `seq` rnf d

-- ------------------------------------------------------------

instance (Binary i, Binary (d c)) => Binary (IndexerState i d c) where
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

instance (XmlPickler i, XmlPickler (d c)) => XmlPickler (IndexerState i d c) where
    xpickle 		= xpElem "index-state" $
			  xpWrap (uncurry IndexerState, \ ix -> (ixs_index ix, ixs_documents ix)) $
			  xpPair xpickle xpickle

-- ------------------------------------------------------------

indexCrawlerConfig		:: (HolIndex i, HolDocuments d c, NFData c) =>
				   Attributes					-- ^ document read options
				-> (URI -> Bool)				-- ^ the filter for deciding, whether the URI shall be processed
				-> Maybe (IOSArrow XmlTree String)		-- ^ the document href collection filter, default is 'Holumbus.DCrawler.Html.getHtmlReferences'
				-> Maybe (IOSArrow XmlTree XmlTree)		-- ^ the pre document filter, default is the this arrow
				-> Maybe (IOSArrow XmlTree String)		-- ^ the filter for computing the document title, default is empty string
				-> Maybe (IOSArrow XmlTree c)			-- ^ the filter for the cutomized doc info, default Nothing
				-> [IndexContextConfig]				-- ^ the configuration of the various index parts
				-> IndexCrawlerConfig i d c			-- ^ result is a crawler config

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


emptyIndexerState		:: i -> d c -> IndexerState i d c
emptyIndexerState eix edm	= IndexerState
				  { ixs_index		= eix
				  , ixs_documents	= edm
				  }

-- ------------------------------------------------------------

insertRawDoc			:: (HolIndex i, HolDocuments d c, NFData c) =>
				   (URI, RawDoc c)				-- ^ extracted URI and doc info
				-> IndexerState i d c				-- ^ old indexer state
				-> IO (IndexerState i d c)			-- ^ new indexer state

insertRawDoc (rawUri, (rawContexts, rawTitle, rawCustom)) ixs
				= return $
				  IndexerState
				  { ixs_index		= newIx
				  , ixs_documents	= newDocs
				  }
    where
    newIx			= foldl' insertRawContext (ixs_index ixs)	-- insert all raw contexts
				  $ (rnf rawContexts `seq` rawContexts)			-- raw context is reduced to normal form

    insertRawContext ix (cx,ws)	= M.foldWithKey insWs ix wpm
	where
	insWs w ps		= insertOccurrences cx w (IM.singleton did ps)
	wpm			= foldl' (flip ins) M.empty $ ws
	ins (w, p)		= M.insertWith IS.union w (IS.singleton p)

    (did, newDocs)		= insertDoc (ixs_documents ixs) (rnf doc `seq` doc)	-- create new doc id and insert doc into documents table
											-- Document is reduced to normal form
    doc				= Document
				  { title	= rawTitle
				  , uri		= rawUri
				  , custom	= rawCustom
				  }

insertRawDocWithId 		:: (HolIndex i, HolDocuments Documents c, NFData c) =>
                                   (URI, RawDoc c)                              -- ^ extracted URI and doc info
                                -> DocId
                                -> IndexerState i Documents c                   -- ^ old indexer state
                                -> IO (IndexerState i Documents c)              -- ^ new indexer state
insertRawDocWithId (rawUri, (rawContexts, rawTitle, rawCustom)) newId ixs
    = return IndexerState { ixs_index   	= newIx
                          , ixs_documents 	= newDocs
                          }
    where
    newIx 			= foldl' insertRawContext (ixs_index ixs) -- insert all raw contexts
                                  $ (rnf rawContexts `seq` rawContexts)         -- raw context is reduced to normal form

    insertRawContext ix (cx,ws) = M.foldWithKey insWs ix wpm
        where
        insWs w ps     		= insertOccurrences cx w (IM.singleton did ps)
        wpm            		= foldl' (flip ins) M.empty $ ws
        ins (w, p)     		= M.insertWith IS.union w (IS.singleton p)

    (did, newDocs) 		= insertDocWithId (ixs_documents ixs) newId (rnf doc `seq` doc)   -- create new doc id and insert doc into documents table
    doc           		= Document
                                  { title   = rawTitle
                                  , uri     = rawUri
                                  , custom  = rawCustom
                                  }
          
insertDocWithId 		:: HolDocuments Documents a => Documents a -> DocId -> (Document a) -> (DocId, Documents a)
insertDocWithId ds newId b 	= maybe reallyInsert (\oldId -> (oldId, ds)) (lookupByURI ds (uri b))
    where
    reallyInsert 		= (newId, Documents newIdToDoc newDocToId newId)
        where
        newIdToDoc 		= IM.insert newId b (idToDoc ds)
        newDocToId 		= M.insert (uri b) newId (docToId ds)

--    newId = (lastDocId ds) + 1          
-- ------------------------------------------------------------

stdIndexer			:: (HolIndex i, HolDocuments d c, Binary c) =>
				   Maybe String					-- ^ resume from interrupted index run with state stored in file
				-> [URI]					-- ^ start indexing with this set of uris
				-> IndexCrawlerConfig i d c			-- ^ adapt configuration to special needs, use id if default is ok
				-> IndexerState i d c				-- ^ the initial empty indexer state
				-> IO (IndexerState i d c)			-- ^ result is a state consisting of the index and the map of indexed documents

stdIndexer resumeLoc startUris config eis
				= do
				  (_, ixState) <- runCrawler action config (initCrawlerState eis)
				  return (getS theResultAccu ixState)
    where
    action			= maybe (crawlDocs startUris) crawlerResume $ resumeLoc

-- ------------------------------------------------------------
{-
-- ------------------------------------------------------------
-- ------------------------------------------------------------

-- experimental: monadic versions of index configuration
-- this becomes interesting when all indexes are instances of HolIndexM m i

indexCrawlerConfigM		:: (HolIndexM IO i, HolDocuments d c, NFData c) =>
				   Attributes					-- ^ document read options
				-> (URI -> Bool)				-- ^ the filter for deciding, whether the URI shall be processed
				-> Maybe (IOSArrow XmlTree String)		-- ^ the document href collection filter, default is 'Holumbus.DCrawler.Html.getHtmlReferences'
				-> Maybe (IOSArrow XmlTree XmlTree)		-- ^ the pre document filter, default is the this arrow
				-> Maybe (IOSArrow XmlTree String)		-- ^ the filter for computing the document title, default is empty string
				-> Maybe (IOSArrow XmlTree c)			-- ^ the filter for the cutomized doc info, default Nothing
				-> [IndexContextConfig]				-- ^ the configuration of the various index parts
				-> IndexCrawlerConfig i d c			-- ^ result is a crawler config

indexCrawlerConfigM opts followRef getHrefF preDocF titleF0 customF0 contextCs
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
				  defaultCrawlerConfig insertRawDocM		-- take the default crawler config
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


insertRawDocM			:: (MonadIO m, HolIndexM m i, HolDocuments d c, NFData c) =>
				   (URI, RawDoc c)				-- ^ extracted URI and doc info
				-> IndexerState i d c				-- ^ old indexer state
				-> m (IndexerState i d c)			-- ^ new indexer state

insertRawDocM (rawUri, (rawContexts, rawTitle, rawCustom)) ixs
				= return $
				  IndexerState
				  { ixs_index		= newIx
				  , ixs_documents	= newDocs
				  }
    where
    newIx			= foldl' insertRawContext (ixs_index ixs) $ rawContexts
    insertRawContext ix cx	= undefined 		-- ix

    (did, newDocs)		= insertDoc (ixs_documents ixs) doc
    doc				= Document					-- Document is reduced to normal form
				  { title	= rawTitle  `using` rnf
				  , uri		= rawUri    `using` rnf
				  , custom	= rawCustom `using` rnf
				  }

-- ------------------------------------------------------------

stdIndexerM			:: (Binary i, HolIndexM IO i, HolDocuments d c, Binary c) =>
				   Maybe String					-- ^ resume from interrupted index run with state stored in file
				-> [URI]					-- ^ start indexing with this set of uris
				-> IndexCrawlerConfig i d c			-- ^ adapt configuration to special needs, use id if default is ok
				-> IndexerState i d c				-- ^ the initial empty indexer state
				-> IO (IndexerState i d c)			-- ^ result is a state consisting of the index and the map of indexed documents

stdIndexerM resumeLoc startUris config eis
				= do
				  (_, ixState) <- runCrawler action config (initCrawlerState eis)
				  return (getS theResultAccu ixState)
    where
    action			= maybe (crawlDocs startUris) crawlerResume $ resumeLoc

-- ------------------------------------------------------------
-}