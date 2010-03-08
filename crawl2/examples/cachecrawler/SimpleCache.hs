{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Data.Function.Selector
-- import           Data.Maybe

import		 Holumbus.Crawler

import           System.Environment

import		 Text.XML.HXT.Arrow		hiding ( readDocument )
import           Text.XML.HXT.Arrow.XmlCache

-- ------------------------------------------------------------

type CacheCrawlerConfig         = CrawlerConfig () CacheState
type CacheState			= ()

-- ------------------------------------------------------------

emptyCacheState               	:: CacheState
emptyCacheState               	= ()

-- ------------------------------------------------------------

stdCacher                      	:: Maybe String                                 -- ^ resume from interrupted index run with state stored in file
                                -> [URI]                                        -- ^ start indexing with this set of uris
                                -> CacheCrawlerConfig                           -- ^ adapt configuration to special needs, use id if default is ok
                                -> CacheState                                   -- ^ the initial empty indexer state
                                -> IO CacheState                                -- ^ result is a state consisting of the index and the map of indexed documents

stdCacher resumeLoc startUris config eis
                                = do
                                  (_, ixState) <- runCrawler action config (initCrawlerState eis)
                                  return (getS theResultAccu ixState)
    where
    action                      = do
                                  noticeC "cacheCore" ["cache update started"]
                                  res <- maybe (crawlDocs startUris) crawlerResume $ resumeLoc
                                  noticeC "cacheCore" ["cache update finished"]
                                  return res

-- ------------------------------------------------------------

-- general HolIndexM IO i version, for old specialized version see code at end of this file

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

unionCacheStatesM		:: (Monad m) => CacheState -> CacheState -> m CacheState
unionCacheStatesM _s1 _s2	= return emptyCacheState

insertCacheM                   	:: (Monad m) => (URI, ()) -> CacheState -> m CacheState
insertCacheM _ _		= return emptyCacheState

-- ------------------------------------------------------------

simpleCacheConfig             	:: (URI -> Bool) -> CacheCrawlerConfig
simpleCacheConfig followRef
				= cacheCrawlerConfig
				  [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
				  , (a_compress, v_1		)			-- cache files will be compressed
				  , (a_document_age,
					 show $ (60 * 60 * 1 * 1::Integer))		-- cache remains valid 1 hour
                                  , (a_accept_mimetypes, 	unwords [text_html, application_xhtml, application_pdf])
				  , (a_parse_html,              v_0)
				  , (a_parse_by_mimetype,	v_1)
				  ]                                                     -- use default read options, but accept pdfs too
				  followRef						-- the set of URIs to be followed and processed 

simpleCacher                   	:: Maybe String						-- resume interupted crawl run
				-> (URI -> Bool)                                        -- uris to be processed
                                -> [URI]                                                -- start uris
                                -> IO CacheState
simpleCacher resume refs startUris
                                = stdCacher
                                  resume
                                  startUris
                                  ( setCrawlerTraceLevel cacherTraceLevel cacherTraceLevelHxt $
                                    setCrawlerSaveConf cacherSaveIntervall cacherSavePath $
                                    setCrawlerMaxDocs cacherMaxDocs cacherMaxParDocs cacherMaxParThreads $
				    simpleCacheConfig refs
                                  )
                                  emptyCacheState

cacherSaveIntervall		:: Int
cacherSaveIntervall		= 200

cacherSavePath			:: String
cacherSavePath			= "./tmp/ix-"

cacherTraceLevel
  , cacherTraceLevelHxt		:: Priority
cacherTraceLevel		= DEBUG -- NOTICE
cacherTraceLevelHxt		= NOTICE

cacherMaxDocs			:: Int
cacherMaxDocs			= 5

cacherMaxParDocs		:: Int
cacherMaxParDocs		= 1

cacherMaxParThreads		:: Int
cacherMaxParThreads		= 1

-- ------------------------------------------------------------

siCacher                       :: Maybe String -> IO CacheState
siCacher resume                = simpleCacher resume refs startUris
    where
    startUris                   = [ "http://www.fh-wedel.de/~si/index.html"
				  ]
    refs                        = simpleFollowRef' [".*"] []
                                  -- [ v1 ++ ".*[.](html|pdf)" ]
{-
                                  ( map (v1 ++) ["welcome[.]html"
						, "handouts/.*.html"
						, ".*[?]VAR=0"
						, "(.*/)?exec[.]html[?].*"
						, ".*/download[a-zA-Z0-9]*[.]html[?].*SRC=.*"
						]
				  )
                                  where
				  v1 = "http://www[.]fh-wedel[.]de/~si/(seminare|klausuren|praktika|projekte|termine|zettelkasten|vorlesungen/(c|cb|fp|internet|java|softwaredesign))/"
-}

-- ------------------------------------------------------------

getOptions                      :: [String] -> (Maybe String, String, String)
getOptions ("-r":fn:as)         = (Just fn, s, r)
                                  where
                                  (_, s, r) = getOptions as
getOptions (uri : out : _)      = (Nothing, uri, out)
getOptions (uri : _)            = (Nothing, uri, "")
getOptions []                   = (Nothing, "", "")

main                            :: IO ()
main                            = do
                                  (resume, _sid, out) <- getArgs >>= return . getOptions
                                  runX ( hxtSetTraceAndErrorLogger NOTICE
                                         >>>
                                         arrIO0 (siCacher resume)
                                         >>>
                                         traceMsg 0 (unwords ["writing cache into XML file", out])
                                         >>>
                                         xpickleDocument xpickle [(a_indent, v_1)] out
                                         >>>
                                         traceMsg 0 "writing cache finished"

                                       )
                                    >> return ()

-- ------------------------------------------------------------
