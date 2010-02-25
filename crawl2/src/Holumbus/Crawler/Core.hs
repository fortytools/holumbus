{-# OPTIONS -XBangPatterns #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Core
    ( module Holumbus.Crawler.Core
    , module Holumbus.Crawler.CrawlerAction
    , module Holumbus.Crawler.Types
    , module Holumbus.Crawler.Logger
    , module Holumbus.Crawler.XmlArrows

    )
where

import           Control.Concurrent.MapFold

import 		 Control.Monad.Reader
import		 Control.Monad.State
import 		 Control.Monad.ReaderStateIO

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State

import           Data.Function.Selector

import           Data.List

import           Holumbus.Crawler.CrawlerAction
import           Holumbus.Crawler.Constants
import		 Holumbus.Crawler.Logger
import           Holumbus.Crawler.URIs
import           Holumbus.Crawler.Robots
import           Holumbus.Crawler.Types
import           Holumbus.Crawler.Util		( mkTmpFile )
import           Holumbus.Crawler.XmlArrows

import		 Text.XML.HXT.Arrow		hiding
                                                ( when
						, getState
						, readDocument
						)
import           Text.XML.HXT.Arrow.XmlCache	( readDocument )

-- import qualified Debug.Trace			as D

-- ------------------------------------------------------------

saveCrawlerState		:: (Binary r) => FilePath -> CrawlerAction c r ()
saveCrawlerState fn		= do
				  s <- get
				  liftIO $ B.encodeFile fn s

loadCrawlerState		:: (Binary r) => FilePath -> CrawlerAction c r ()
loadCrawlerState fn		= do
				  s <- liftIO $ B.decodeFile fn
				  put s


uriProcessed			:: URI -> CrawlerAction c r ()
uriProcessed uri		= do
				  modifyState theToBeProcessed    $ deleteURI uri
				  modifyState theAlreadyProcessed $ insertURI uri

urisProcessed			:: URIs -> CrawlerAction c r ()
urisProcessed uris		= do
				  modifyState theToBeProcessed    $ deleteURIs uris
				  modifyState theAlreadyProcessed $ flip unionURIs  uris

uriToBeProcessed		:: URI -> CrawlerAction c r ()
uriToBeProcessed uri		= do
				  aps <- getState theAlreadyProcessed
				  when ( not $ uri `memberURIs` aps )
				       ( modifyState theToBeProcessed $ insertURI uri )

urisToBeProcessed		:: URIs -> CrawlerAction c r ()
urisToBeProcessed uris		= do
				  aps <- getState theAlreadyProcessed
                                  let newUris = deleteURIs aps uris
				  modifyState theToBeProcessed $ flip unionURIs newUris

uriAddToRobotsTxt		:: URI -> CrawlerAction c r ()
uriAddToRobotsTxt uri		= do
				  raa <- getConf theAddRobotsAction
				  modifyStateIO theRobots (raa uri)

accumulateRes			:: (URI, c) -> CrawlerAction c r ()
accumulateRes res		= do
				  combine <- getConf  theAccumulateOp
				  acc0    <- getState theResultAccu
				  acc1    <- liftIO $ combine res acc0
				  putState theResultAccu acc1

-- ------------------------------------------------------------

crawlDocs		:: Binary r => [URI] -> CrawlerAction c r ()
crawlDocs uris		= do
			  noticeC "crawlDocs" ["crawlDocs: init crawler state and start crawler loop"]
			  putState theToBeProcessed (fromListURIs uris)
			  crawlerLoop

crawlerLoop		:: Binary r => CrawlerAction c r ()
crawlerLoop		= do
			  p <- getConf theMaxParDocs
			  n <- getState   theNoOfDocs
			  m <- getConf theMaxNoOfDocs
			  when (n /= m)
			       ( do
				 noticeC "crawlerLoop" ["iteration", show $ n+1]
				 tbp <- getState theToBeProcessed
			         noticeC "crawlerLoop" [show $ cardURIs tbp, "uri(s) remain to be processed"]
				 when (not . nullURIs $ tbp)
				      ( do
					if p <= 0		-- no parallel crawling
					  then crawlNextDoc
				          else crawlNextDocs
					crawlerSaveState
					crawlerLoop
				      )
			       )

crawlerResume		:: Binary r => String -> CrawlerAction c r ()
crawlerResume fn	= do
			  noticeC "crawlerResume" ["read crawler state from", fn]
			  loadCrawlerState fn
			  noticeC "crawlerResume" ["resume crawler"]
			  crawlerLoop

crawlerSaveState	:: Binary r => CrawlerAction c r ()
crawlerSaveState	= do
			  n1 <- getState theNoOfDocs
                          n0 <- getState theNoOfDocsSaved
			  m  <- getConf  theSaveIntervall
			  when ( m > 0 && n1 - n0 >= m)
			       ( do
				 fn <- getConf theSavePathPrefix
				 let fn' = mkTmpFile 10 fn n1
				 noticeC "crawlerSaveState" [show n1, "documents into", show fn']
                                 putState theNoOfDocsSaved n1
				 saveCrawlerState fn'
				 noticeC "crawlerSaveState" ["saving state finished"]
			       )

-- ------------------------------------------------------------

crawlNextDocs		:: CrawlerAction c r ()
crawlNextDocs		= do
		          uris <- getState theToBeProcessed
                          n <- getConf theMaxParDocs
                          let urisTBP = nextURIs n uris
			  modifyState theNoOfDocs (+ (length urisTBP))
                          noticeC "crawlNextDocs" ["next", show (length urisTBP), "uri(s) will be processed"]
                          urisProcessed $ fromListURIs urisTBP
                          urisAllowed <- filterM isAllowedByRobots urisTBP
                          when (not . null $ urisAllowed) $
                               do
                               conf  <- ask
                               state <- get
                               (urisMoved, urisNew, results) <- liftIO $
                                                                mapFold n (processCmd conf state) combineDocResults $
                                                                urisAllowed
                               noticeC "crawlNextDocs" [show . cardURIs $ urisNew, "hrefs found, accumulating results"]
                               urisProcessed     urisMoved
                               urisToBeProcessed urisNew
                               mapM_ accumulateRes results	-- this is not yet the best solution
                               					-- combining the results could be done in mapFold
                               noticeC "crawlNextDocs" ["document results accumulated"]

    where
    processCmd c s u	= do
			  (res, _) <- runCrawler (processDoc' u) c s
			  res `seq` return res
    processDoc' u	= do
                          noticeC "processDoc" ["URI to be processed: ", show u]
                          conf <- ask
			  [(!u', (!uris', !docRes))] <- liftIO $ runX (processDocArrow conf u)
                          let toBeFollowed = getS theFollowRef conf
                          let !movedUris    = if null u'
                                              then emptyURIs
                                              else singletonURIs u'
                          let !newUris      = fromListURIs .
                                              filter toBeFollowed $ uris'
                          let !docRes'      = docRes
                          noticeC "processDoc" ["processed: ", show u] 
                          return $! (movedUris, newUris, docRes')

    combineDocResults (m1, n1, r1) (m2, n2, r2)
			= return (m, n, r)
	where
        !m 		= unionURIs m1 m2
        !n 		= unionURIs n1 n2
        !r 		= r1 ++ r2

-- ------------------------------------------------------------

-- | crawl a single doc, mark doc as processed, collect new hrefs and combine doc result with accumulator in state

crawlNextDoc		:: CrawlerAction c r ()
crawlNextDoc		= do
		          uris <- getState theToBeProcessed
			  modifyState theNoOfDocs (+1)
                          let uri = nextURI uris
			  noticeC "crawlNextDoc" [show uri]
			  uriProcessed      uri					-- uri is put into processed URIs
                          isGood <- isAllowedByRobots uri
                          when isGood $
			    do
		            (uri', uris', resList') <- processDoc uri		-- get document and extract new refs and result
			    when (not . null $ uri') $
			      uriProcessed uri'					-- doc has been moved, uri' is real uri, so it's also put into the set of processed URIs
			    noticeC "crawlNextDoc" [show . length . nub . sort $ uris', "new uris found"]
			    mapM_ uriToBeProcessed uris'			-- insert new uris into toBeProcessed set
			    mapM_ accumulateRes resList'			-- combine results with state accu

-- | Run the process document arrow and prepare results

processDoc		:: URI -> CrawlerAction c r (URI, [URI], [(URI, c)])
processDoc uri		= do
			  conf <- ask
			  [(uri', (uris, res))] <- liftIO $ runX (processDocArrow conf uri)
			  return ( if uri' /= uri
				   then uri'
				   else ""
				 , filter (getS theFollowRef conf) uris
				 , res							-- usually in case of normal processing this list consists of a singleton list
				 )
							-- and in case of an error it's an empty list
-- ------------------------------------------------------------

-- | filter uris rejected by robots.txt

isAllowedByRobots	:: URI -> CrawlerAction c r Bool
isAllowedByRobots uri	= do
                          uriAddToRobotsTxt uri						-- for the uri host, a robots.txt is loaded, if neccessary
                          rdm <- getState theRobots
                          if (robotsDisallow rdm uri)					-- check, whether uri is disallowed by host/robots.txt
			     then do
				  noticeC "isAllowedByRobots" ["uri rejected by robots.txt", show uri]
				  return False
                             else return True

-- ------------------------------------------------------------

-- | From a document two results are computed, 1. the list of all hrefs in the contents,
-- and 2. the collected info contained in the page. This result is augmented with the transfer uri
-- such that following functions know the source of this contents. The transfer-URI may be another one
-- as the input uri, there could happen a redirect in the http request.
--
-- The two listA arrows make the whole arrow deterministic, so it never fails

processDocArrow		:: CrawlerConfig c r -> URI -> IOSArrow a (URI, ([URI], [(URI, c)]))
processDocArrow c uri	= ( hxtSetTraceAndErrorLogger WARNING
			    >>>
			    readDocument (getS theReadAttributes c) uri
			    >>>
			    perform ( ( getAttrValue transferStatus
					&&&
				        getAttrValue transferMessage
				      )
				      >>>
				      ( arr2 $ \ s m -> unwords ["processDocArrow: response code:", s, m] )
				      >>>
				      traceString 1 id
				    )
			    >>>
			    ( getRealDocURI
			      &&&
			      listA ( checkDocumentStatus
				      >>>
				      getS thePreRefsFilter c
				      >>>
				      getS theProcessRefs c
				    )
			      &&&
			      listA (  getS thePreDocFilter c
				       >>>
				       ( getAttrValue transferURI
					 &&&
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
				  getAttrValue0 http_location

-- | compute the real URI of the document, in case of a move response
-- this is contained in the \"http-location\" attribute, else it's the
-- tranferURI.

getRealDocURI			:: ArrowXml a => a XmlTree String
getRealDocURI			= fromLA $
				  getLocationReference
				  `orElse`
				  getAttrValue transferURI

-- ------------------------------------------------------------

initCrawler			:: CrawlerAction c r ()
initCrawler			= do
				  conf <- ask
				  setLogLevel "" (getS theTraceLevel conf)
				  

runCrawler			:: CrawlerAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO (x, CrawlerState r)
runCrawler a			= runReaderStateIO (initCrawler >> a)

-- run a crawler and deliver just the accumulated result value

execCrawler			:: CrawlerAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO r
execCrawler cmd config initState
				= do
				  (_, finalState) <- runCrawler cmd config initState
				  return (getS theResultAccu finalState)

-- ------------------------------------------------------------
