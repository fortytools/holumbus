{-# LANGUAGE BangPatterns #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Core
where

import           Control.Concurrent.MapFold       (mapFold)
import           Control.DeepSeq
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.ReaderStateIOError
import           Control.Monad.State
import           Control.Sequential.MapFoldBinary (mapFoldBinaryM)

import           Data.Binary                      (Binary)
import qualified Data.Binary                      as B
import           Data.Function.Selector
import           Data.List

import           Holumbus.Crawler.Constants
import           Holumbus.Crawler.Logger
import           Holumbus.Crawler.Robots
import           Holumbus.Crawler.Types
import           Holumbus.Crawler.URIs
import           Holumbus.Crawler.Util            (mkTmpFile)
import           Holumbus.Crawler.XmlArrows

import           Text.XML.HXT.Core                hiding (getState, when)

-- ------------------------------------------------------------
-- error safe IO

stdIOErrorHandler               :: IOError -> CrawlerAction a r ()
stdIOErrorHandler e             = do errC "IO" [show e]

-- perform an IO action and catch and issue IO errors

liftIOE                         :: IO b -> CrawlerAction a r b
liftIOE action                  = liftIO action `catchError`
                                  (\ e -> errC "IO" [e] >> throwError e)

-- ------------------------------------------------------------

saveCrawlerState                :: (Binary r) => FilePath -> CrawlerAction a r ()
saveCrawlerState fn             = do preSave
                                     s <- get
                                     liftIOE $ B.encodeFile  fn s
    where
      preSave                   = do act <- getConf theSavePreAction
                                     act fn

loadCrawlerState                :: (Binary r) => FilePath -> CrawlerAction a r ()
loadCrawlerState fn             = do s <- liftIOE $ B.decodeFile fn
                                     put s

uriProcessed                    :: URI -> CrawlerAction a r ()
uriProcessed uri                = do
                                  modifyState theToBeProcessed    $ deleteURI uri
                                  modifyState theAlreadyProcessed $ insertURI uri

urisProcessed                   :: URIs -> CrawlerAction a r ()
urisProcessed uris              = do
                                  modifyState theToBeProcessed    $ deleteURIs uris
                                  modifyState theAlreadyProcessed $ flip unionURIs  uris

uriToBeProcessed                :: URI -> Int -> CrawlerAction a r ()
uriToBeProcessed uri level      = do
                                  aps <- getState theAlreadyProcessed
                                  when ( not $ uri `memberURIs` aps )
                                       ( modifyState theToBeProcessed $ insertURI' uri level)

urisToBeProcessed               :: URIsWithLevel -> CrawlerAction a r ()
urisToBeProcessed uris          = do
                                  aps <- getState theAlreadyProcessed
                                  let newUris = deleteURIs aps uris
                                  modifyState theToBeProcessed $ flip (unionURIs' min) newUris

uriAddToRobotsTxt               :: URI -> CrawlerAction a r ()
uriAddToRobotsTxt uri           = do
                                  conf <- ask
                                  let raa = getS theAddRobotsAction conf
                                  modifyStateIO theRobots (raa conf uri)

accumulateRes                   :: (NFData r) => (URI, a) -> CrawlerAction a r ()
accumulateRes res               = do
                                  combine <- getConf  theAccumulateOp
                                  acc0    <- getState theResultAccu
                                  acc1    <- liftIOE $ combine res acc0
                                  putState theResultAccu acc1

-- ------------------------------------------------------------

crawlDocs               :: (NFData a, NFData r, Binary r) => [URI] -> CrawlerAction a r ()
crawlDocs uris          = do
                          noticeC "crawlDocs" ["init crawler state and start crawler loop"]
                          putState theToBeProcessed (fromListURIs' $ zip uris (repeat 0))
                          crawlerLoop
                          noticeC "crawlDocs" ["crawler loop finished"]
                          crawlerSaveState

crawlerLoop             :: (NFData a, NFData r, Binary r) => CrawlerAction a r ()
crawlerLoop             = do
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
                                        case t of
                                          0         -> crawlNextDoc                     -- sequential crawling
                                          1         -> crawlNextDocs mapFoldBinaryM     -- sequential crawling with binary mapFold
                                          _         -> crawlNextDocs (mapFold t)        -- parallel mapFold crawling
                                        crawlerCheckSaveState
                                        crawlerLoop
                                      )
                               )

crawlerResume           :: (NFData a, NFData r, Binary r) => String -> CrawlerAction a r ()
crawlerResume fn        = do
                          noticeC "crawlerResume" ["read crawler state from", fn]
                          loadCrawlerState fn
                          noticeC "crawlerResume" ["resume crawler"]
                          crawlerLoop

crawlerCheckSaveState   :: Binary r => CrawlerAction a r ()
crawlerCheckSaveState   = do
                          n1 <- getState theNoOfDocs
                          n0 <- getState theNoOfDocsSaved
                          m  <- getConf  theSaveIntervall
                          when ( m > 0 && n1 - n0 >= m)
                               crawlerSaveState

crawlerSaveState        :: Binary r => CrawlerAction a r ()
crawlerSaveState        = do
                          n1 <- getState theNoOfDocs
                          n0 <- getState theNoOfDocsSaved
                          when (n1 > n0)           -- else state has already been saved, don't do it twice
                               ( do
                                 fn <- getConf theSavePathPrefix
                                 let fn' = mkTmpFile 10 fn n1
                                 noticeC "crawlerSaveState" [show n1, "documents into", show fn']
                                 putState theNoOfDocsSaved n1
                                 modifyState theListOfDocsSaved (n1 :)
                                 saveCrawlerState fn'
                                 noticeC "crawlerSaveState" ["saving state finished"]
                               )

-- ------------------------------------------------------------

type MapFold a r        = (a -> IO r) -> (r -> r -> IO r) -> [a] -> IO r

crawlNextDocs           :: (NFData r) => MapFold URIWithLevel (URIs, URIsWithLevel, r) -> CrawlerAction a r ()
crawlNextDocs mapf      = do
                          uris <- getState theToBeProcessed
                          nd   <- getState   theNoOfDocs

                          mp   <- getConf  theMaxParDocs
                          md   <- getConf theMaxNoOfDocs

                          let n       = mp `min` (md - nd)
                          let urisTBP = nextURIs n uris

                          modifyState theNoOfDocs (+ (length urisTBP))

                          noticeC "crawlNextDocs" ["next", show (length urisTBP), "uri(s) will be processed"]

                          urisProcessed $ fromListURIs $ map fst urisTBP
                          urisAllowed <- filterM (isAllowedByRobots . fst) urisTBP

                          when (not . null $ urisAllowed) $
                               do
                               conf     <- ask
                               let mergeOp = getS theFoldOp conf

                               state' <- get
                               ( ! urisMoved,
                                 ! urisNew,
                                 ! results
                                 )      <- liftIOE $
                                           mapf (processCmd conf state') (combineDocResults' mergeOp) $
                                           urisAllowed

                               noticeC "crawlNextDocs" [show . cardURIs $ urisNew, "hrefs found, accumulating results"]
                               mapM_ (debugC "crawlNextDocs") $ map (("href" :) . (:[])) $ toListURIs urisNew
                               urisProcessed     urisMoved
                               urisToBeProcessed urisNew
                               acc0     <- getState theResultAccu
                               acc1     <- liftIOE $ mergeOp results acc0
                               putState theResultAccu acc1
                               noticeC "crawlNextDocs" ["document results accumulated"]

    where
      -- processCmd runs in the IO monad
    processCmd c s u    = do noticeC "processCmd"     ["processing document:", show u]
                             (res, _s') <- runCrawler (processDoc' u) c s
                             either
                               ( ioError . userError )
                               ( \ (m1, n1, rawRes) ->
                                 do r1 <- foldM (flip accOp) res0 rawRes
                                    rnf r1 `seq` rnf m1 `seq` rnf r1 `seq`
                                        noticeC "processCmd" ["document processed: ", show u]
                                    return (m1, n1, r1)
                               ) res
        where
        res0            = getS theResultInit   s
        accOp           = getS theAccumulateOp c

-- ------------------------------------------------------------

processDoc'             :: URIWithLevel -> CrawlerAction a r (URIs, URIsWithLevel, [(URI, a)])
processDoc' (uri, lev)  = do
                          conf <- ask
                          [(uri', (uris', docRes))] <- liftIOE $ runX (processDocArrow conf uri)
                          let toBeFollowed = getS theFollowRef conf
                          let maxLevel     = getS theClickLevel conf
                          let ! lev1       = lev + 1
                          let movedUris    = if null uri'
                                             then emptyURIs
                                             else singletonURIs uri'
                          let newUris      = if lev >= maxLevel
                                             then emptyURIs
                                             else fromListURIs'
                                                  . map (\ u -> (u, lev1))
                                                  . filter toBeFollowed
                                                  $ uris'
                          return (movedUris, newUris, docRes)

-- ------------------------------------------------------------

combineDocResults'      :: (NFData r) =>
                           MergeDocResults r ->
                          (URIs, URIsWithLevel, r) ->
                          (URIs, URIsWithLevel, r) -> IO (URIs, URIsWithLevel, r)
combineDocResults' mergeOp (m1, n1, r1) (m2, n2, r2)
                        = do
                          noticeC "crawlNextDocs" ["combining results"]
                          r     <- mergeOp r1 r2
                          m     <- return $ unionURIs m1 m2
                          n     <- return $ unionURIs' min n1 n2
                          res   <- return $ (m, n, r)
                          rnf res `seq`
                              noticeC "crawlNextDocs" ["results combined"]
                          return res

-- ------------------------------------------------------------
--
-- | crawl a single doc, mark doc as processed, collect new hrefs and combine doc result with accumulator in state

crawlNextDoc            :: (NFData a, NFData r) => CrawlerAction a r ()
crawlNextDoc            = do
                          uris <- getState theToBeProcessed
                          modifyState theNoOfDocs (+1)
                          let uri@(u, _lev) = nextURI uris
                          noticeC "crawlNextDoc" [show uri]
                          uriProcessed u                                        -- uri is put into processed URIs
                          isGood <- isAllowedByRobots u
                          when isGood $
                            do
                            res <- processDoc uri                               -- get document and extract new refs and result
                            let (uri', uris', resList') = rnf res `seq` res     -- force evaluation

                            when (not . null $ uri') $
                              uriProcessed uri'                                 -- doc has been moved, uri' is real uri, so it's also put into the set of processed URIs

                            noticeC "crawlNextDoc" [show . length . nub . sort $ uris', "new uris found"]
                            mapM_ (uncurry uriToBeProcessed) uris'              -- insert new uris into toBeProcessed set
                            mapM_ accumulateRes resList'                        -- combine results with state accu

-- | Run the process document arrow and prepare results

processDoc              :: URIWithLevel -> CrawlerAction a r (URI, [URIWithLevel], [(URI, a)])
processDoc (uri, lev)   = do
                          conf <- ask
                          let maxLevel = getS theClickLevel conf
                          let ! lev1   = lev + 1
                          [(uri', (uris, res))] <- liftIOE $ runX (processDocArrow conf uri)
                          let newUris  = if lev >= maxLevel
                                         then []
                                         else map (\ u -> (u, lev1))
                                              . filter (getS theFollowRef conf)
                                              $ uris
                          return ( if uri' /= uri
                                   then uri'
                                   else ""
                                 , newUris
                                 , res                  -- usually in case of normal processing this is a singleton list
                                 )
                                                        -- and in case of an error it's an empty list
-- ------------------------------------------------------------

-- | filter uris rejected by robots.txt

isAllowedByRobots       :: URI -> CrawlerAction a r Bool
isAllowedByRobots uri   = do
                          uriAddToRobotsTxt uri         -- for the uri host, a robots.txt is loaded, if neccessary
                          rdm <- getState theRobots
                          if (robotsDisallow rdm uri)   -- check, whether uri is disallowed by host/robots.txt
                             then do
                                  noticeC "isAllowedByRobot" ["uri rejected by robots.txt", show uri]
                                  return False
                             else do
                                  -- noticeC "isAllowedByRobot" ["uri allowed  by robots.txt", show uri]
                                  return True

-- ------------------------------------------------------------

-- | From a document two results are computed, 1. the list of all hrefs in the contents,
-- and 2. the collected info contained in the page. This result is augmented with the transfer uri
-- such that following functions know the source of this contents. The transfer-URI may be another one
-- as the input uri, there could happen a redirect in the http request.
--
-- The two listA arrows make the whole arrow deterministic, so it never fails

processDocArrow         :: CrawlerConfig c r -> URI -> IOSArrow a (URI, ([URI], [(URI, c)]))

processDocArrow c uri   = ( hxtSetTraceAndErrorLogger (getS theTraceLevelHxt c)
                            >>>
                            readDocument [getS theSysConfig c] uri
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

getLocationReference            :: ArrowXml a => a XmlTree String
getLocationReference            = fromLA $
                                  ( getAttrValue0 transferStatus
                                    >>>
                                    isA (`elem` ["301", "302"])
                                  )
                                  `guards`
                                  getAttrValue0 http_location

-- | compute the real URI of the document, in case of a move response
-- this is contained in the \"http-location\" attribute, else it's the
-- tranferURI.

getRealDocURI                   :: ArrowXml a => a XmlTree String
getRealDocURI                   = fromLA $
                                  getLocationReference
                                  `orElse`
                                  getAttrValue transferURI

-- ------------------------------------------------------------

initCrawler                     :: CrawlerAction a r ()
initCrawler                     = do
                                  conf <- ask
                                  setLogLevel "" (getS theTraceLevel conf)


runCrawler                      :: CrawlerAction a r x ->
                                   CrawlerConfig a r   ->
                                   CrawlerState r      -> IO (Either String x, CrawlerState r)
runCrawler a                    =  runReaderStateIOError (initCrawler >> a)

-- run a crawler and deliver just the accumulated result value

execCrawler                     :: CrawlerAction a r x ->
                                   CrawlerConfig a r   ->
                                   CrawlerState r      -> IO (Either String (CrawlerState r))
execCrawler cmd config initState
                                = do (r, s) <- runCrawler cmd config initState
                                     return $ either Left (const $ Right s) r


-- ------------------------------------------------------------
