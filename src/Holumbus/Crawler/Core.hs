{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Core
where

import 		 Control.Monad.Reader
import		 Control.Monad.State
import 		 Control.Monad.ReaderStateIO

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State

import           Data.Function.Selector

import           Data.List
import		 Data.Maybe

import qualified Data.Set       		as S

import qualified Holumbus.Index.Common		as H
                                                ( URI
						, writeToBinFile
						, loadFromBinFile
						)
import           Holumbus.Crawler.Robots
import           Holumbus.Crawler.Util		( mkTmpFile )

import           System.IO

import		 Text.XML.HXT.Arrow		hiding
                                                ( when
						, getState
						)

-- import qualified Debug.Trace			as D

-- ------------------------------------------------------------

type URI			= H.URI

-- | A set of URIs
type URIs			= S.Set URI

-- | The action to combine the result of a single document with the accumulator for the overall crawler result.
-- This combining function runs in the IO monad to enable storing parts of the result externally

type AccumulateDocResult a r	= (URI, a) -> r -> IO r

-- | The crawler configuration record

data CrawlerConfig a r		= CrawlerConfig
                                  { cc_readAttributes	:: ! Attributes
				  , cc_preRefsFilter	:: IOSArrow XmlTree XmlTree
				  , cc_processRefs	:: IOSArrow XmlTree URI
				  , cc_preDocFilter     :: IOSArrow XmlTree XmlTree
				  , cc_processDoc	:: IOSArrow XmlTree a
				  , cc_accumulate	:: AccumulateDocResult a r		-- result accumulation runs in the IO monad to allow storing parts externally
				  , cc_followRef	:: URI -> Bool
				  , cc_maxNoOfDocs	:: ! Int
				  , cc_saveIntervall	:: ! Int
				  , cc_savePathPrefix	:: ! String
				  , cc_traceLevel	:: ! Int
				  }

-- | The crawler state record

data CrawlerState r		= CrawlerState
                                  { cs_toBeProcessed    :: ! URIs
				  , cs_alreadyProcessed :: ! URIs
				  , cs_robots		:: ! Robots				-- is part of the state, it will grow during crawling
				  , cs_noOfDocs		:: ! Int				-- stop crawling when this counter reaches 0, (-1) means unlimited # of docs
				  , cs_resultAccu       :: ! r					-- evaluate accumulated result, else memory leaks show up
				  }
				  deriving (Show)

type CrawlerAction a r x	= ReaderStateIO (CrawlerConfig a r) (CrawlerState r) x

-- ------------------------------------------------------------

-- | selector functions for CrawlerState

theToBeProcessed	:: Selector (CrawlerState r) URIs
theToBeProcessed	= S cs_toBeProcessed	(\ x s -> s {cs_toBeProcessed = x})

theAlreadyProcessed	:: Selector (CrawlerState r) URIs
theAlreadyProcessed	= S cs_alreadyProcessed	(\ x s -> s {cs_alreadyProcessed = x})

theRobots		:: Selector (CrawlerState r) Robots
theRobots		= S cs_robots	(\ x s -> s {cs_robots = x})

theNoOfDocs		:: Selector (CrawlerState r) Int
theNoOfDocs		= S cs_noOfDocs	(\ x s -> s {cs_noOfDocs = x})

theResultAccu		:: Selector (CrawlerState r) r
theResultAccu		= S cs_resultAccu	(\ x s -> s {cs_resultAccu = x})

-- | selector functions for CrawlerConfig

theReadAttributes	:: Selector (CrawlerConfig a r) Attributes
theReadAttributes	= S cc_readAttributes	(\ x s -> s {cc_readAttributes = x})

theTraceLevel		:: Selector (CrawlerConfig a r) Int
theTraceLevel		= S cc_traceLevel	(\ x s -> s {cc_traceLevel = x})

theMaxNoOfDocs		:: Selector (CrawlerConfig a r) Int
theMaxNoOfDocs		= S cc_maxNoOfDocs	(\ x s -> s {cc_maxNoOfDocs = x})

theSaveIntervall	:: Selector (CrawlerConfig a r) Int
theSaveIntervall	= S cc_saveIntervall	(\ x s -> s {cc_saveIntervall = x})

theSavePathPrefix	:: Selector (CrawlerConfig a r) String
theSavePathPrefix	= S cc_savePathPrefix	(\ x s -> s {cc_savePathPrefix = x})

theFollowRef		:: Selector (CrawlerConfig a r) (URI -> Bool)
theFollowRef		= S cc_followRef	(\ x s -> s {cc_followRef = x})

theAccumulateOp		:: Selector (CrawlerConfig a r) (AccumulateDocResult a r)
theAccumulateOp		= S cc_accumulate	(\ x s -> s {cc_accumulate = x})

thePreRefsFilter	:: Selector (CrawlerConfig a r) (IOSArrow XmlTree XmlTree)
thePreRefsFilter	= S cc_preRefsFilter	(\ x s -> s {cc_preRefsFilter = x})

theProcessRefs		:: Selector (CrawlerConfig a r) (IOSArrow XmlTree URI)
theProcessRefs		= S cc_processRefs	(\ x s -> s {cc_processRefs = x})

thePreDocFilter		:: Selector (CrawlerConfig a r) (IOSArrow XmlTree XmlTree)
thePreDocFilter		= S cc_preDocFilter	(\ x s -> s {cc_preDocFilter = x})

theProcessDoc		:: Selector (CrawlerConfig a r) (IOSArrow XmlTree a)
theProcessDoc		= S cc_processDoc	(\ x s -> s {cc_processDoc = x})

-- ------------------------------------------------------------

-- a rather boring default crawler configuration

defaultCrawlerConfig	:: AccumulateDocResult a r -> CrawlerConfig a r
defaultCrawlerConfig op	= CrawlerConfig
			  { cc_readAttributes	= [ (curl_user_agent,		defaultCrawlerName)
						  , (curl_max_time,		show $ (60 * 1000::Int))	-- whole transaction for reading a document must complete within 60,000 mili seconds, 
						  , (curl_connect_timeout,	show $ (10::Int))	 	-- connection must be established within 10 seconds
						  ]
			  , cc_preRefsFilter	= this						-- no preprocessing for refs extraction
			  , cc_processRefs	= none						-- don't extract refs
			  , cc_preDocFilter     = this						-- no document preprocessing
			  , cc_processDoc	= none						-- no document processing at all
			  , cc_accumulate	= op						-- combining function for result accumulating
			  , cc_followRef	= const False					-- do not follow any refs
			  , cc_traceLevel	= 1						-- traceLevel
			  , cc_saveIntervall	= (-1)						-- never save an itermediate state
			  , cc_savePathPrefix	= "/tmp/hc-"					-- the prefix for filenames into which intermediate states are saved
			  , cc_maxNoOfDocs	= (-1)						-- maximum number of docs to be crawled, -1 means unlimited
			  }

defaultCrawlerName	:: String
defaultCrawlerName	= "HolumBot/0.2 @http://holumbus.fh-wedel.de -" ++ "-location"

curl_user_agent		:: String
curl_user_agent		= "curl-" ++ "-user-agent"

curl_max_time		:: String
curl_max_time           = "curl-" ++ "-max-time"

curl_connect_timeout	:: String
curl_connect_timeout	= "curl-" ++ "-connect-timeout"

curl_max_filesize	:: String
curl_max_filesize	= "curl-" ++ "-max-filesize"

curl_location		:: String
curl_location		= "curl-" ++ "-location"

theCrawlerName		:: Selector (CrawlerConfig a r) String
theCrawlerName		= theReadAttributes
			  >>>
			  S { getS = lookupDef defaultCrawlerName curl_user_agent
			    , setS = addEntry curl_user_agent
			    }

theMaxTime		:: Selector (CrawlerConfig a r) Int
theMaxTime		= theReadAttributes
			  >>>
			  S { getS = read . lookupDef "0" curl_max_time
			    , setS = addEntry curl_max_time . show . (`max` 1)
			    }

theConnectTimeout	:: Selector (CrawlerConfig a r) Int
theConnectTimeout	= theReadAttributes
			  >>>
			  S { getS = read . lookupDef "0" curl_connect_timeout
			    , setS = addEntry curl_connect_timeout . show . (`max` 1)
			    }


-- ------------------------------------------------------------

addReadAttributes	:: Attributes -> CrawlerConfig a r -> CrawlerConfig a r
addReadAttributes al	= update theReadAttributes (addEntries al)

-- | Insert a robots no follow filter before thePreRefsFilter

addRobotsNoFollow	:: CrawlerConfig a r -> CrawlerConfig a r
addRobotsNoFollow	= update thePreRefsFilter ( robotsNoFollow >>> )

-- | Insert a robots no follow filter before thePreRefsFilter

addRobotsNoIndex	:: CrawlerConfig a r -> CrawlerConfig a r
addRobotsNoIndex	= update thePreDocFilter ( robotsNoIndex >>> )

-- ------------------------------------------------------------

instance (Binary r) => Binary (CrawlerState r) where
    put	s		= do
			  B.put (getS theToBeProcessed s)
			  B.put (getS theAlreadyProcessed s)
			  B.put (getS theRobots s)
			  B.put (getS theNoOfDocs s)
			  B.put (getS theResultAccu s)
    get			= do
			  tbp <- B.get
			  alp <- B.get
			  rbt <- B.get
			  mxd <- B.get
			  acc <- B.get
			  return $ CrawlerState
				   { cs_toBeProcessed    = tbp
				   , cs_alreadyProcessed = alp
				   , cs_robots           = rbt
				   , cs_noOfDocs         = mxd
				   , cs_resultAccu       = acc
				   }

putCrawlerState		:: (Binary r) => CrawlerState	r -> B.Put
putCrawlerState		= B.put

getCrawlerState		:: (Binary r) => B.Get (CrawlerState r)
getCrawlerState		= B.get

initCrawlerState	:: r -> CrawlerState r
initCrawlerState r	= CrawlerState
			  { cs_toBeProcessed    = emptyURIs
			  , cs_alreadyProcessed = emptyURIs
			  , cs_robots		= emptyRobots
			  , cs_noOfDocs		= 0
			  , cs_resultAccu	= r
			  }

-- ------------------------------------------------------------

emptyURIs		:: URIs
emptyURIs		= S.empty

nullURIs		:: URIs -> Bool
nullURIs		= S.null

memberURIs		:: URI -> URIs -> Bool
memberURIs		= S.member

cardURIs		:: URIs -> Int
cardURIs		= S.size

nextURI			:: URIs -> URI
nextURI			= S.findMin

insertURI		:: URI -> URIs	-> URIs
insertURI		= S.insert

deleteURI		:: URI -> URIs	-> URIs
deleteURI		= S.delete

fromListURIs		:: [URI] -> URIs
fromListURIs		= S.fromList

foldURIs		:: (URI -> b -> b) -> b -> URIs -> b
foldURIs		= S.fold

-- ------------------------------------------------------------
--
-- basic crawler actions

-- | Load a component from the crawler configuration

getConf				:: Selector (CrawlerConfig a r) v -> CrawlerAction a r v
getConf				= asks . getS

getState			:: Selector (CrawlerState r) v -> CrawlerAction a r v
getState 			= gets . getS

putState			:: Selector (CrawlerState r) v -> v -> CrawlerAction a r ()
putState sel			= modify . setS sel

modifyState			:: Selector (CrawlerState r) v -> (v -> v) -> CrawlerAction a r ()
modifyState sel			= modify . update sel

traceCrawl			:: Int -> [String] -> CrawlerAction c r ()
traceCrawl l msg		= do
				  l0 <- getConf theTraceLevel
				  when ( l <= l0 )
                                       ( liftIO $ hPutStrLn stderr $ "-" ++ "- (" ++ show l ++ ") " ++ unwords msg )

saveCrawlerState		:: (Binary r) => FilePath -> CrawlerAction c r ()
saveCrawlerState fn		= do
				  s <- get
				  liftIO $ H.writeToBinFile fn s

loadCrawlerState		:: (Binary r) => FilePath -> CrawlerAction c r ()
loadCrawlerState fn		= do
				  s <- liftIO $ H.loadFromBinFile fn
				  put s


uriProcessed			:: URI -> CrawlerAction c r ()
uriProcessed uri		= do
				  modifyState theToBeProcessed    $ deleteURI uri
				  modifyState theAlreadyProcessed $ insertURI uri

uriToBeProcessed		:: URI -> CrawlerAction c r ()
uriToBeProcessed uri		= do
				  aps <- getState theAlreadyProcessed
				  when ( not $ uri `memberURIs` aps )
				       ( modifyState theToBeProcessed $ insertURI uri )

accumulateRes			:: (URI, c) -> CrawlerAction c r ()
accumulateRes res		= do
				  combine <- getConf  theAccumulateOp
				  acc0    <- getState theResultAccu
				  acc1    <- liftIO $ combine res acc0
				  putState theResultAccu acc1
			  
-- ------------------------------------------------------------

crawlDocs		:: Binary r => [URI] -> CrawlerAction c r ()
crawlDocs uris		= do
			  traceCrawl 1 ["crawlDocs: init crawler state and start crawler loop"]
			  putState theToBeProcessed (fromListURIs uris)
			  crawlerLoop

crawlerLoop		:: Binary r => CrawlerAction c r ()
crawlerLoop		= do
			  n <- getState   theNoOfDocs
			  m <- getConf theMaxNoOfDocs
			  when (n /= m)
			       ( do
				 traceCrawl 1 ["crawlerLoop: iteration", show $ n+1]
				 modifyState theNoOfDocs (+1)
				 tbp <- getState theToBeProcessed
			         traceCrawl 1 ["crawlerLoop:", show $ cardURIs tbp, "uri(s) to be processed"]
				 when (not . nullURIs $ tbp)
				      ( do
					crawlDoc $ nextURI tbp
					crawlerSaveState
					crawlerLoop
				      )
			       )

crawlerResume		:: Binary r => String -> CrawlerAction c r ()
crawlerResume fn	= do
			  traceCrawl 1 ["crawlerResume: read crawler state from", fn]
			  loadCrawlerState fn
			  traceCrawl 1 ["crawlerResume: resume crawler"]
			  crawlerLoop

crawlerSaveState	:: Binary r => CrawlerAction c r ()
crawlerSaveState	= do
			  n <- getState   theNoOfDocs
			  m <- getConf theSaveIntervall
			  when ( m > 0 && n `mod` m == 0)
			       ( do
				 fn <- getConf theSavePathPrefix
				 let fn' = mkTmpFile 2 fn (n `div` m)			-- 2 digits: last 100 files are saved, 1 digit: last 10 file
				 traceCrawl 1 [ "crawlerSaveState: saving state for"
					      , show n, "documents into", show fn'
					      ]
				 saveCrawlerState fn'
				 traceCrawl 1 ["crawlerSaveState: saving state finished"]
			       )

-- | crawl a single doc, mark doc as proessed, collect new hrefs and combine doc result with accumulator in state

crawlDoc		:: URI -> CrawlerAction c r ()
crawlDoc uri		= do
			  traceCrawl 1 ["crawlDoc:", show uri]
			  uriProcessed uri						-- uri is put into processed URIs
			  (uri', uris, res) <- processDoc uri				-- get document and extract new refs and result
			  when (not . null $ uri') $
			       uriProcessed uri'					-- doc has been moved, uri' is real uri, so it's also put into the set of processed URIs

			  traceCrawl 1 ["crawlDoc: new uris:", show . nub . sort $ uris]
			  mapM_ uriToBeProcessed uris					-- insert new uris into toBeProcessed set
			  maybe (return ()) accumulateRes res				-- combine result with state accu

-- | Run the process document arrow and prepare results

processDoc		:: URI -> CrawlerAction c r (URI, [URI], Maybe (URI, c))
processDoc uri		= do
			  conf <- ask
			  [(uri', (uris, res))] <- liftIO $ runX (processDocArrow conf uri)
			  return ( if uri' /= uri
				   then uri'
				   else ""
				 , filter (getS theFollowRef conf) uris
				 , listToMaybe res
				 )

-- | From a document two results are computed, 1. the list of all hrefs in the contents,
-- and 2. the collected info contained in the page. This result is augmented with the transfer uri
-- such that following functions know the source of this contents. The transfer-URI may be another one
-- as the input uri, there could happen a redirect in the http request.
--
-- The two listA arrows make the whole arrow deterministic, so it never fails

processDocArrow		:: CrawlerConfig c r -> URI -> IOSArrow a (URI, ([URI], [(URI, c)]))
processDocArrow c uri	= ( setTraceLevel ( (getS theTraceLevel c) - 1)
			    >>>
			    readDocument (getS theReadAttributes c) uri
			    >>>
			    setTraceLevel (getS theTraceLevel c)
			    >>>
			    perform ( ( getAttrValue transferStatus
					&&&
				        getAttrValue transferMessage
				      )
				      >>>
				      ( arr2 $ \ s m -> unwords ["crawlDoc: response code:", s, m] )
				      >>>
				      traceString 1 id
				    )
			    >>>
			    ( getRealDocURI
			      &&&
			      listA ( ( replaceChildren none
					`whenNot`
					documentStatusOk
				      )					-- only the contents of "good" documents are searched for refs
				      >>>
				      getS thePreRefsFilter c
				      >>>
				      getS theProcessRefs c
				    )
			      &&&
			      listA ( getAttrValue transferURI
				      &&&
				      ( getS thePreDocFilter c
					>>>
					getS theProcessDoc c
				      )
				    )
			    )
			  )
			  `withDefault` ("", ([], []))

-- ------------------------------------------------------------

-- | compute the real URI in case of a 301 or 302 response (moved permanently or temporary),
-- else the arrow will fail

getLocationReference		:: ArrowXml a => a XmlTree String
getLocationReference		= fromLA $
				  ( getAttrValue0 transferStatus
				    >>>
				    isA (`elem` ["301", "302"])
				  )
				  `guards`
				  getAttrValue0 "http-location"

-- | compute the real URI of the document, in case of a move response
-- this is contained in the \"http-location\" attribute, else it's the
-- tranferURI.

getRealDocURI			:: ArrowXml a => a XmlTree String
getRealDocURI			= fromLA $
				  getLocationReference
				  `orElse`
				  getAttrValue transferURI

-- ------------------------------------------------------------

runCrawler			:: CrawlerAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO (x, CrawlerState r)
runCrawler			= runReaderStateIO

-- run a crawler and deliver just the accumulated result value

execCrawler			:: CrawlerAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO r
execCrawler cmd config initState
				= do
				  (_, finalState) <- runCrawler cmd config initState
				  return (getS theResultAccu finalState)

-- ------------------------------------------------------------
