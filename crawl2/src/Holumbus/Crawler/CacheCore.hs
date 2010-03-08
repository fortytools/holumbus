{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.CacheCore
where

import           Control.DeepSeq

import           Data.Binary			( Binary(..) )
{-
import qualified Data.Binary			as B
-}

import           Data.Function.Selector

import		 Holumbus.Crawler

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

type CacheCrawlerConfig         = CrawlerConfig () CacheState

newtype CacheState		= CS ()

-- ------------------------------------------------------------

-- If CacheState was declared as alias for () this would be redundant,
-- but this can be taken as a pattern for other crawlers

instance NFData CacheState where

instance Binary CacheState where
    put 		= const $ return ()
    get			= return emptyCacheState

instance XmlPickler CacheState where
    xpickle		= xpElem "cacheState" $
                          xpWrap (const emptyCacheState, const ()) $
                          xpUnit

-- ------------------------------------------------------------

emptyCacheState               	:: CacheState
emptyCacheState               	= CS ()

-- ------------------------------------------------------------

unionCacheStatesM		:: (Monad m) => CacheState -> CacheState -> m CacheState
unionCacheStatesM _s1 _s2	= return emptyCacheState

insertCacheM                   	:: (Monad m) => (URI, ()) -> CacheState -> m CacheState
insertCacheM _ _		= return emptyCacheState

-- ------------------------------------------------------------

-- the cache crawler configureation

cacheCrawlerConfig              :: Attributes                                   -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> CacheCrawlerConfig                           -- ^ result is a crawler config

cacheCrawlerConfig opts followRef
                                = addReadAttributes defaultOpts                 -- install the default read options
                                  >>>
                                  addReadAttributes opts                        -- overwrite and add specific read options
                                  >>>
                                  ( setS theFollowRef followRef )
                                  >>>
                                  ( setS theProcessRefs getHtmlReferences )
                                  >>>
                                  ( setS thePreDocFilter checkDocumentStatus )  -- in case of errors throw away any contents
                                  >>>
                                  ( setS theProcessDoc  $ constA ())
                                  >>>
                                  enableRobotsTxt                               -- add the robots stuff at the end
                                  >>>                                           -- the filter wrap the other filters
                                  addRobotsNoFollow
                                  >>>
                                  addRobotsNoIndex
                                  $
                                  defaultCrawlerConfig insertCacheM unionCacheStatesM
									        -- take the default crawler config
                                                                                -- and set the result combining functions
    where
    defaultOpts                 = [ (curl_max_filesize,         "1000000")      -- limit document size to 1 Mbyte
                                  , (curl_location,             v_1)            -- automatically follow redirects
                                  , (curl_max_redirects,        "3")            -- but limit # of redirects to 3
                                  , (a_accept_mimetypes,        "text/html text/xhtml text/plain text/pdf")
                                  , (a_encoding,                isoLatin1)
                                  , (a_ignore_encoding_errors,  v_1)            -- encoding errors and parser warnings are boring
                                  , (a_validate,                v_0)
                                  , (a_parse_html,              v_1)
                                  , (a_issue_warnings,  	v_0)
                                  ]

-- ------------------------------------------------------------

stdCacher			:: (Int, Int, Int)				-- ^ the parameters for parallel crawling 
                                -> (Int, String)				-- ^ the save intervall and file path
                                -> (Priority, Priority)				-- ^ the log levels for the crawler and hxt
                                -> Attributes					-- ^ the read attributes
                                -> (CacheCrawlerConfig -> CacheCrawlerConfig)	-- ^ further configuration settings
                                -> Maybe String                                 -- ^ resume from interrupted index run with state stored in file
                                -> [URI]                                        -- ^ start caching with this set of uris
                                -> (URI -> Bool) -> IO CacheState

stdCacher (maxDocs, maxParDocs, maxParThreads)
          (saveIntervall, savePath)
          (trc, trcx)
          inpOptions
          furtherConfigs
          resumeLoc
          startUris
          followRef		= do
			          (_, cState) <- runCrawler action config initState
			          return (getS theResultAccu cState)
    where
    initState			= initCrawlerState emptyCacheState
    action			= do
                                  noticeC "cacheCore" ["cache update started"]
				  maybe (crawlDocs startUris) crawlerResume $ resumeLoc
                                  noticeC "cacheCore" ["cache update finished"]

    config			= setCrawlerMaxDocs maxDocs maxParDocs maxParThreads
			          >>>
			          setCrawlerSaveConf saveIntervall savePath
			          >>>
			          setCrawlerTraceLevel trc trcx
			          >>>
			          enableRobotsTxt						-- change to disableRobotsTxt, when robots.txt becomes boring
                                  >>>
                                  furtherConfigs
			          $
			          cacheCrawlerConfig inpOptions followRef

-- ------------------------------------------------------------
