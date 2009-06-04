{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XNoMonomorphismRestriction -XImpredicativeTypes #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Applicative

import 		 Control.Monad.Reader
import		 Control.Monad.State
import 		 Control.Monad.ReaderStateIO

-- import		 Control.Parallel.Strategies

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State

import           Data.Function.Selector

import           Data.List
import		 Data.Maybe

import qualified Data.Map       		as M
import qualified Data.Set       		as S

import           Holumbus.Index.Common		( writeToBinFile
						, loadFromBinFile
						)
import           Holumbus.Crawler.Robots
import           Holumbus.Index.Common          ( URI )

import           System.IO

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )
import qualified Text.XML.HXT.Arrow		as X
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
						( match )

-- ------------------------------------------------------------

type URIs		= S.Set URI

data CrawlerConfig a r	= CrawlerConfig
                          { cc_readAttributes	:: ! Attributes
			  , cc_preRefsFilter	:: IOSArrow XmlTree XmlTree
			  , cc_processRefs	:: IOSArrow XmlTree URI
			  , cc_preDocFilter     :: IOSArrow XmlTree XmlTree
			  , cc_processDoc	:: IOSArrow XmlTree a
			  , cc_accumulate	:: (URI, a) -> r -> r
			  , cc_followRef	:: URI -> Bool
			  , cc_maxNoOfDocs	:: ! Int
			  , cc_traceLevel	:: ! Int
			  }

data CrawlerState r	= CrawlerState
                          { cs_toBeProcessed    :: ! URIs
			  , cs_alreadyProcessed :: ! URIs
			  , cs_robots		:: ! Robots				-- is part of the state, it will grow during crawling
			  , cs_noOfDocs		:: ! Int				-- stop crawling when this counter reaches 0, (-1) means unlimited # of docs
			  , cs_resultAccu       :: r
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

theFollowRef		:: Selector (CrawlerConfig a r) (URI -> Bool)
theFollowRef		= S cc_followRef	(\ x s -> s {cc_followRef = x})

theAccumulateOp		:: Selector (CrawlerConfig a r) ((URI, a) -> r -> r)
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

defaultCrawlerConfig	:: ((URI, a) -> r -> r) -> CrawlerConfig a r
defaultCrawlerConfig op	= CrawlerConfig
			  { cc_readAttributes	= [ (curl_user_agent,		defaultCrawlerName)
						  -- , (curl_max_time,		"60")		-- whole transaction for reading a document must complete within 60 seconds
						  -- , (curl_connect_timeout,	"10000")		-- connection must be established within 10 seconds
						  ]
			  , cc_preRefsFilter	= this						-- no preprocessing for refs extraction
			  , cc_processRefs	= none						-- don't extract refs
			  , cc_preDocFilter     = this						-- no document preprocessing
			  , cc_processDoc	= none						-- no document processing at all
			  , cc_accumulate	= op						-- combining function for result accumulating
			  , cc_followRef	= const False					-- do not follow any refs
			  , cc_traceLevel	= 1						-- traceLevel
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


theCrawlerName		:: Selector (CrawlerConfig a r) String
theCrawlerName		= theReadAttributes
			  >>>
			  S { load  = lookupDef defaultCrawlerName curl_user_agent
			    , store = addEntry curl_user_agent
			    }

theMaxTime		:: Selector (CrawlerConfig a r) Int
theMaxTime		= theReadAttributes
			  >>>
			  S { load  = read . lookupDef "0" curl_max_time
			    , store = addEntry curl_max_time . show . (`max` 1)
			    }

theConnectTimeout	:: Selector (CrawlerConfig a r) Int
theConnectTimeout	= theReadAttributes
			  >>>
			  S { load  = read . lookupDef "0" curl_connect_timeout
			    , store = addEntry curl_connect_timeout . show . (`max` 1)
			    }


-- ------------------------------------------------------------

addReadAttributes	:: Attributes -> CrawlerConfig a r -> CrawlerConfig a r
addReadAttributes al	= update theReadAttributes (addEntries al)

-- ------------------------------------------------------------

instance (Binary r) => Binary (CrawlerState r) where
    put	s		= B.put (load theToBeProcessed s)
			  >>
			  B.put (load theAlreadyProcessed s)
			  >>
			  B.put (load theRobots s)
			  >>
			  B.put (load theNoOfDocs s)
			  >>
			  B.put (load theResultAccu s)
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
			  , cs_noOfDocs		= (-1)			-- unlimited
			  , cs_resultAccu	= r
			  }

decrTheNoOfDocs		:: CrawlerState r -> CrawlerState r
decrTheNoOfDocs		= update theNoOfDocs (\ x -> (x - 1) `max` (-1))

-- ------------------------------------------------------------

emptyURIs		:: URIs
emptyURIs		= S.empty

nullURIs		:: URIs -> Bool
nullURIs		= S.null

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

-- ------------------------------------------------------------
--
-- basic crawler actions

-- | Load a component from the crawler configuration

getConf			:: Selector (CrawlerConfig a r) v -> CrawlerAction a r v
getConf			= asks . load

getState		:: Selector (CrawlerState r) v -> CrawlerAction a r v
getState 		= gets . load

putState		:: Selector (CrawlerState r) v -> v -> CrawlerAction a r ()
putState sel		= modify . store sel

traceCrawl		:: Int -> String -> CrawlerAction c r ()
traceCrawl l msg	= do
			  l0 <- getConf theTraceLevel
			  when (l >= l0) $ liftIO $ hPutStrLn stderr $ "-" ++ "- (" ++ show l ++ ") " ++ msg

saveCrawlerState	:: (Binary r) => FilePath -> CrawlerAction c r ()
saveCrawlerState fn	= do
			  s <- get
			  liftIO $ writeToBinFile fn s

loadCrawlerState	:: (Binary r) => FilePath -> CrawlerAction c r ()
loadCrawlerState fn	= do
			  s <- liftIO $ loadFromBinFile fn
			  put s


uriProcessed		:: URI -> CrawlerAction c r ()
uriProcessed uri	= modify $
			  ( update theToBeProcessed    (deleteURI uri)
			    >>>
			    update theAlreadyProcessed (insertURI uri)
			  )

uriToBeProcessed		:: URI -> CrawlerAction c r ()
uriToBeProcessed uri		= modify uriToBeProcessed'
    where
    uriToBeProcessed' s
	| alreadyProcessed	= s
	| otherwise		= update theToBeProcessed (insertURI uri) s
	where
	alreadyProcessed	= S.member uri . load theAlreadyProcessed $ s


accumulateRes			:: (URI, c) -> CrawlerAction c r ()
accumulateRes res		= do
				  combine <- getConf theAccumulateOp
				  modify (update theResultAccu (combine res))
			  
-- ------------------------------------------------------------

crawlDocs		:: [URI] -> CrawlerAction c r ()
crawlDocs uris		= do
			  putState theToBeProcessed (fromListURIs uris)
			  n <- getConf theMaxNoOfDocs
			  putState theNoOfDocs n
			  crawlerLoop

crawlerLoop		:: CrawlerAction c r ()
crawlerLoop		= do
			  n <- getState theNoOfDocs
			  when (n /= 0)
			       ( do
				 traceCrawl 1 $ unwords ["crawlerLoop:", show n, "iteration(s) left"]
				 modify decrTheNoOfDocs
				 tbp <- getState theToBeProcessed
			         traceCrawl 1 $ unwords ["crawlerLoop:", show $ cardURIs tbp, "uri to be processed"]
				 when (not . nullURIs $ tbp)
				      ( do
					crawlDoc $ nextURI tbp
					crawlerLoop
				      )
			       )

-- | crawl a single doc, mark doc as proessed, collect new hrefs and combine doc result with accumulator in state

crawlDoc		:: URI -> CrawlerAction c r ()
crawlDoc uri		= do
			  traceCrawl 1 $ "crawlDoc: " ++ show uri
			  uriProcessed uri				-- uri is put into processed URIs
			  (uris, res) <- processDoc uri			-- get document and extract new refs and result

			  traceCrawl 1 $ "crawlDoc: uris: " ++ show (nub . sort $ uris)
			  mapM_ uriToBeProcessed uris			-- insert new uris into toBeProcessed set
			  maybe (return ()) accumulateRes res		-- combine result with state accu

processDoc		:: URI -> CrawlerAction c r ([URI], Maybe (URI, c))
processDoc uri		= do
			  conf <- ask
			  [(uris, res)] <- liftIO $ runX (processDocArrow conf uri)
			  return ( filter (load theFollowRef conf) uris
				 , listToMaybe res
				 )
-- | From a document two results are computed, 1. the list of all hrefs in the contents,
-- and 2. the collected info contained in the page. This result is augmented with the transfer uri
-- such that following functions know the source of this contents. The transfer-URI may be another one
-- as the input uri, there could happen a redirect in the http request.
--
-- The two listA arrows make the whole arrow deterministic, and it will never fail

processDocArrow		:: CrawlerConfig c r -> URI -> IOSArrow a ([URI], [(URI, c)])
processDocArrow c uri	= ( readDocument (load theReadAttributes c) uri
			    >>>
			    setTraceLevel (load theTraceLevel c)
			    >>>
			    ( listA ( load thePreRefsFilter c
				      >>>
				      load theProcessRefs c
				    )
			      &&&
			      listA ( getAttrValue "transfer-URI"
				      &&&
				      ( load thePreDocFilter c
					>>>
					load theProcessDoc c
				      )
				    )
			    )
			  )
			  `withDefault` ([], [])

-- ------------------------------------------------------------

runCrawler			:: CrawlerAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO (x, CrawlerState r)
runCrawler			= runReaderStateIO

-- run a crawler and deliver just the accumulated result value

execCrawler			:: CrawlerAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO r
execCrawler cmd config initState
				= do
				  (_, finalState) <- runCrawler cmd config initState
				  return (load theResultAccu finalState)

-- ------------------------------------------------------------

simpleFollowRef			:: (String -> Bool) -> (String -> Bool) -> (String -> Bool)
simpleFollowRef isAllowed isDenied
    				= isAllowed .&&. (not . isDenied)
				  where
				  (.&&.) = liftA2 (&&)

simpleFollowRef'		:: [String] -> [String] -> (String -> Bool)
simpleFollowRef' allowed denied
				= simpleFollowRef (match $ mkAlt allowed) (match $ mkAlt denied)
    where
    mkAlt			:: [String] -> String
    mkAlt rs			= "(" ++ intercalate "|" rs ++ ")"


-- ------------------------------------------------------------

defaultHtmlCrawlerConfig	:: ((URI, a) -> r -> r) -> CrawlerConfig a r
defaultHtmlCrawlerConfig op	= ( addReadAttributes
				    [ (a_validate,   		 v_0)
				    , (a_parse_html,		 v_1)
				    , (a_encoding,		 isoLatin1)
				    , (a_issue_warnings, 	 v_0)
				    , (a_ignore_none_xml_contents, v_1)
				    ]
				    >>>
				    store thePreRefsFilter this
				    >>>
				    store theProcessRefs getHtmlReferences
				    $  defaultCrawlerConfig op
				  )

-- ------------------------------------------------------------

getHtmlReferences 		:: ArrowXml a => a XmlTree URI
getHtmlReferences		= getRefs' $< computeDocBase
    where
    getRefs' base	        = fromLA $
				  deep (hasNameWith ((`elem` ["a","frame","iframe"]) . localPart))
				  >>>
				  ( getAttrValue0 "href"
				    <+>
				    getAttrValue0 "src"
				  )
				  >>^ (toAbsRef base)

toAbsRef        		:: URI -> URI -> URI
toAbsRef base ref		= ( expandURIString ref			-- here >>> is normal function composition
				    >>>
				    fromMaybe ref
				    >>>
				    removeFragment
				  ) base
    where
    removeFragment r
        | "#" `isPrefixOf` path = reverse . tail $ path
        | otherwise 		= r
        where
        path 			= dropWhile (/='#') . reverse $ r 

-- | Compute the base of a webpage
--   stolen from Uwe Schmidt, http:\/\/www.haskell.org\/haskellwiki\/HXT
--   and then stolen back again from Holumbus.Utility

computeDocBase  		:: ArrowXml a => a XmlTree String
computeDocBase			= ( ( ( this				-- try to find a base element in head
					/> hasName "html"		-- and compute document base with transfer uri and base
					/> hasName "head"
					/> hasName "base"
					>>> getAttrValue "href"
				      )
				      &&&
				      getAttrValue "transfer-URI"
				    )
				    >>> expandURI
				  )
				  `orElse`
				  getAttrValue "transfer-URI"  		-- the default: take the transfer uri
      

-- ------------------------------------------------------------
--
-- a test application

type TextDoc		= String

type TextDocs		= M.Map URI TextDoc

type HtmlCrawlerConfig	= CrawlerConfig TextDoc TextDocs

emptyTextDocs		:: TextDocs
emptyTextDocs		= M.empty

insertTextDoc		:: (URI, TextDoc) -> TextDocs -> TextDocs
insertTextDoc		= uncurry M.insert

textHtmlCrawlerConfig	:: HtmlCrawlerConfig
textHtmlCrawlerConfig	= ( addReadAttributes  [ (a_validate,   		 v_0)
					       , (a_parse_html,		 v_1)
					       , (a_encoding,		 isoLatin1)
					       , (a_issue_warnings, 		 v_0)
					       , (a_ignore_none_xml_contents, v_1)
					       ]
			    >>>
			    store theFollowRef (const True)
			    >>>
			    store theProcessDoc (fromLA (rnfA extractText))	-- force complete evaluation of the result
			    $ defaultHtmlCrawlerConfig insertTextDoc
			  )
    where
    extractText		= xshow ( ( theTitle <+> theBody )
				  >>>
				  deep isText
				)
			  >>^ (words >>> unwords)

    theBody		= this /> hasName "html" /> hasName "body"
    theTitle		= this /> hasName "html" /> hasName "head" /> hasName "title"

textHtmlCrawlerInitState	:: CrawlerState TextDocs
textHtmlCrawlerInitState	= initCrawlerState emptyTextDocs

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig TextDoc TextDocs
testCrawlerConfig	= store theFollowRef ( simpleFollowRef'
			                       ["http://localhost/~si/klausuren/.*[.]html"]
			                       ["()"]
					     )
			  >>>
			  store theMaxNoOfDocs 10
			  $ textHtmlCrawlerConfig

t1 	:: IO TextDocs
t1	= execCrawler (crawlDoc "http://localhost/~si/") testCrawlerConfig textHtmlCrawlerInitState

t2 	:: IO ((), CrawlerState TextDocs)
t2	= runCrawler  (crawlDocs ["http://localhost/~si/"]) testCrawlerConfig textHtmlCrawlerInitState

-- ------------------------------------------------------------
