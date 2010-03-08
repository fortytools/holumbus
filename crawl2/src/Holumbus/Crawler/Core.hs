{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Core
where

import           Control.Concurrent.MapFold
import           Control.DeepSeq

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

-- ------------------------------------------------------------

saveCrawlerState		:: (Binary r) => FilePath -> CrawlerAction a r ()
saveCrawlerState fn		= do
				  s <- get
				  liftIO $ B.encodeFile fn s

loadCrawlerState		:: (Binary r) => FilePath -> CrawlerAction a r ()
loadCrawlerState fn		= do
				  s <- liftIO $ B.decodeFile fn
				  put s


uriProcessed			:: URI -> CrawlerAction a r ()
uriProcessed uri		= do
				  modifyState theToBeProcessed    $ deleteURI uri
				  modifyState theAlreadyProcessed $ insertURI uri

urisProcessed			:: URIs -> CrawlerAction a r ()
urisProcessed uris		= do
				  modifyState theToBeProcessed    $ deleteURIs uris
				  modifyState theAlreadyProcessed $ flip unionURIs  uris

uriToBeProcessed		:: URI -> CrawlerAction a r ()
uriToBeProcessed uri		= do
				  aps <- getState theAlreadyProcessed
				  when ( not $ uri `memberURIs` aps )
				       ( modifyState theToBeProcessed $ insertURI uri )

urisToBeProcessed		:: URIs -> CrawlerAction a r ()
urisToBeProcessed uris		= do
				  aps <- getState theAlreadyProcessed
                                  let newUris = deleteURIs aps uris
				  modifyState theToBeProcessed $ flip unionURIs newUris

uriAddToRobotsTxt		:: URI -> CrawlerAction a r ()
uriAddToRobotsTxt uri		= do
                                  conf <- ask
				  let raa = getS theAddRobotsAction conf
				  modifyStateIO theRobots (raa conf uri)

accumulateRes			:: (URI, a) -> CrawlerAction a r ()
accumulateRes res		= do
				  combine <- getConf  theAccumulateOp
				  acc0    <- getState theResultAccu
				  acc1    <- liftIO $ combine res acc0
				  putState theResultAccu acc1

-- ------------------------------------------------------------

crawlDocs		:: (NFData a, NFData r, Binary r) => [URI] -> CrawlerAction a r ()
crawlDocs uris		= do
			  noticeC "crawlDocs" ["init crawler state and start crawler loop"]
			  putState theToBeProcessed (fromListURIs uris)
			  crawlerLoop
			  noticeC "crawlDocs" ["crawler loop finished"]
                          crawlerSaveState

crawlerLoop		:: (NFData a, NFData r, Binary r) => CrawlerAction a r ()
crawlerLoop		= do
			  n <- getState   theNoOfDocs
			  m <- getConf theMaxNoOfDocs
                          t <- getConf theMaxParThreads
			  when (n < m)
			       ( do
				 noticeC "crawlerLoop" ["iteration", show $ n+1]
				 tbp <- getState theToBeProcessed
			         noticeC "crawlerLoop" [show $ cardURIs tbp, "uri(s) remain to be processed"]
				 when (not . nullURIs $ tbp)
				      ( do
					if t <= 0
					  then crawlNextDoc	-- sequential crawling
				          else crawlNextDocs    -- parallel mapFold crawling
					crawlerCheckSaveState
					crawlerLoop
				      )
			       )

crawlerResume		:: (NFData a, NFData r, Binary r) => String -> CrawlerAction a r ()
crawlerResume fn	= do
			  noticeC "crawlerResume" ["read crawler state from", fn]
			  loadCrawlerState fn
			  noticeC "crawlerResume" ["resume crawler"]
			  crawlerLoop

crawlerCheckSaveState	:: Binary r => CrawlerAction a r ()
crawlerCheckSaveState	= do
			  n1 <- getState theNoOfDocs
                          n0 <- getState theNoOfDocsSaved
			  m  <- getConf  theSaveIntervall
			  when ( m > 0 && n1 - n0 >= m)
                               crawlerSaveState
crawlerSaveState	:: Binary r => CrawlerAction a r ()
crawlerSaveState	= do
			  n1 <- getState theNoOfDocs
			  fn <- getConf theSavePathPrefix
			  let fn' = mkTmpFile 10 fn n1
			  noticeC "crawlerSaveState" [show n1, "documents into", show fn']
                          putState theNoOfDocsSaved n1
			  saveCrawlerState fn'
			  noticeC "crawlerSaveState" ["saving state finished"]

-- ------------------------------------------------------------

crawlNextDocs		:: (NFData r) => CrawlerAction a r ()
crawlNextDocs		= do
		          uris <- getState theToBeProcessed
                          n <- getConf theMaxParDocs
                          t <- getConf theMaxParThreads
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
                                                                mapFold t (processCmd conf state) (combineDocResults conf) $
                                                                urisAllowed
                               noticeC "crawlNextDocs" [show . cardURIs $ urisNew, "hrefs found, accumulating results"]
                               mapM_ (debugC "crawlNextDocs") $ map (("href" :) . (:[])) $ toListURIs urisNew
                               urisProcessed     urisMoved
                               urisToBeProcessed urisNew
                               acc0 <- getState theResultAccu
                               acc1 <- liftIO $ (getS theFoldOp conf) results acc0
                               putState theResultAccu acc1
                               noticeC "crawlNextDocs" ["document results accumulated"]

    where
    processCmd c s u	= do
			  ((m1, n1, rawRes), _) <- runCrawler (processDoc' u) c s
                          r1 <- foldM (flip accOp) res0 rawRes
                          return (m1, n1, r1)
        where
        res0		= getS theResultInit   s
        accOp		= getS theAccumulateOp c

    processDoc' u	= do
                          noticeC "processDoc" ["processing:", show u]

                          conf <- ask
			  [(u', (uris', docRes))] <- liftIO $ runX (processDocArrow conf u)
                          let toBeFollowed = getS theFollowRef conf
                          let movedUris    = if null u'
                                             then emptyURIs
                                             else singletonURIs u'
                          let newUris      = fromListURIs .
                                             filter toBeFollowed $ uris'

                          noticeC "processDoc" ["processed :", show u] 
                          return (movedUris, newUris, docRes)

    combineDocResults c (m1, n1, r1) (m2, n2, r2)
			= do
                          r <- mergeOp r1 r2
			  return (m, n, r)
	where
        m 		= unionURIs m1 m2
        n 		= unionURIs n1 n2
        mergeOp		= getS theFoldOp c

-- ------------------------------------------------------------

-- | crawl a single doc, mark doc as processed, collect new hrefs and combine doc result with accumulator in state

crawlNextDoc		:: (NFData a) => CrawlerAction a r ()
crawlNextDoc		= do
		          uris <- getState theToBeProcessed
			  modifyState theNoOfDocs (+1)
                          let uri = nextURI uris
			  noticeC "crawlNextDoc" [show uri]
			  uriProcessed      uri					-- uri is put into processed URIs
                          isGood <- isAllowedByRobots uri
                          when isGood $
			    do
		            res <- processDoc uri				-- get document and extract new refs and result
		            (uri', uris', resList') <- rnf res `seq`		-- force evaluation
                                                       return res
			    when (not . null $ uri') $
			      uriProcessed uri'					-- doc has been moved, uri' is real uri, so it's also put into the set of processed URIs
			    noticeC "crawlNextDoc" [show . length . nub . sort $ uris', "new uris found"]
			    mapM_ uriToBeProcessed uris'			-- insert new uris into toBeProcessed set
			    mapM_ accumulateRes resList'			-- combine results with state accu

-- | Run the process document arrow and prepare results

processDoc		:: URI -> CrawlerAction a r (URI, [URI], [(URI, a)])
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

isAllowedByRobots	:: URI -> CrawlerAction a r Bool
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

processDocArrow c uri	= ( hxtSetTraceAndErrorLogger (getS theTraceLevelHxt c)
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

initCrawler			:: CrawlerAction a r ()
initCrawler			= do
				  conf <- ask
				  setLogLevel "" (getS theTraceLevel conf)
				  

runCrawler			:: CrawlerAction a r x -> CrawlerConfig a r -> CrawlerState r -> IO (x, CrawlerState r)
runCrawler a			= runReaderStateIO (initCrawler >> a)

-- run a crawler and deliver just the accumulated result value

execCrawler			:: CrawlerAction a r x -> CrawlerConfig a r -> CrawlerState r -> IO r
execCrawler cmd config initState
				= do
				  (_, finalState) <- runCrawler cmd config initState
				  return (getS theResultAccu finalState)

-- ------------------------------------------------------------
