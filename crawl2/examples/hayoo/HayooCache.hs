{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import		 Data.Function.Selector		( update )
import		 Holumbus.Crawler
import		 Holumbus.Crawler.CacheCore

import           System.Environment

import		 Text.XML.HXT.Arrow	hiding	( readDocument ) 
import           Text.XML.HXT.Arrow.XmlCache
import		 Text.XML.HXT.XPath

import		 HayooConfig

-- ------------------------------------------------------------
{-
simpleCacher			:: Maybe String -> [URI] -> (URI -> Bool) -> IO CacheState
simpleCacher			= stdCacher
                                  (15000, 100, 10)					-- max docs, max par docs, max threads
                                  (500, "./tmp/ix-")					-- save intervall and path
                                  (DEBUG, NOTICE)					-- log cache and hxt
				  [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
				  , (a_compress, v_1		)			-- cache files will be compressed
				  , (a_document_age,
					 show $ (60 * 60 * 24 * 1::Int))		-- cache remains valid 1 day
                                  , (a_accept_mimetypes, 	unwords [text_html, application_xhtml])
				  , (a_parse_html,              v_0)
				  , (a_parse_by_mimetype,	v_1)
				  ]                                                     -- use default read options, but accept pdfs too
				  ( editPackageURIs					-- configure URI rewriting
                                    >>>
                                    disableRobotsTxt					-- for hayoo robots.txt is not needed
                                  )
-}

crawlDoc			:: (Int, Int, Int)
crawlSav			:: (Int, String)
crawlLog			:: (Priority, Priority)
crawlPar			:: [(String, String)]
crawlFct			:: CacheCrawlerConfig -> CacheCrawlerConfig

crawlDoc			= (15000, 100, 10)					-- max docs, max par docs, max threads
crawlSav			= (500, "./tmp/ix-")					-- save intervall and path
crawlLog			= (DEBUG, NOTICE)					-- log cache and hxt
crawlPar			= [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
				  , (a_compress, v_1		)			-- cache files will be compressed
				  , (a_document_age,
					 show $ (60 * 60 * 24 * 1::Integer))		-- cache remains valid 1 day
                                  , (a_accept_mimetypes, 	unwords [text_html, application_xhtml])
				  , (a_parse_html,              v_0)
				  , (a_parse_by_mimetype,	v_1)
				  ]
crawlFct			= ( editPackageURIs					-- configure URI rewriting
                                    >>>
                                    disableRobotsTxt					-- for hayoo robots.txt is not needed
                                  )

-- ------------------------------------------------------------

editPackageURIs			:: CacheCrawlerConfig -> CacheCrawlerConfig
editPackageURIs			= update theProcessRefs (>>> arr editLatestPackage)

-- ------------------------------------------------------------

getRecentPackages		:: IOSArrow b String
getRecentPackages		= readDocument [ (a_validate, v_0)
					       ] "http://hackage.haskell.org/packages/archive/recent.rss"
				  >>>
				  getXPathTrees "/rss/channel/item/title"
				  >>>
				  xshow (deep isText)
				  >>>
				  arr (words >>> take 1 >>> concat)

-- ------------------------------------------------------------

hayooCacher 			:: Maybe String -> IO CacheState
hayooCacher resume              = stdCacher crawlDoc crawlSav crawlLog crawlPar crawlFct resume hayooStart (hayooRefs [])

-- ------------------------------------------------------------

hayooPackageUpdate		:: [String] -> IO CacheState
hayooPackageUpdate pkgs		= stdCacher crawlDoc crawlSav crawlLog crawlPar' crawlFct Nothing hayooStart (hayooRefs pkgs)
    where
    crawlPar'			= addEntries [(a_document_age, show $ (1 * 1 * 1 * 1::Int))] crawlPar		-- cache validation initiated (1 sec valid) 

-- ------------------------------------------------------------

getOptions                      :: [String] -> (Maybe [String], Maybe String, String, String)
getOptions ("-r":fn:as)         = (Nothing, Just fn, s, r)
                                  where
                                  (_, _, s, r) = getOptions as
getOptions ("-u":pns:as)	= (Just pnl, Nothing, s, r)
				  where
				  (_, _, s, r) = getOptions as
				  pnl = words . map (\ x -> if x == ',' then ' ' else x) $ pns
getOptions (uri : out : _)      = (Nothing, Nothing, uri, out)
getOptions (uri : _)            = (Nothing, Nothing, uri, "")
getOptions []                   = (Nothing, Nothing, "", "")

main                            :: IO ()
main                            = do
                                  (updpkg, resume, _sid, out) <- getArgs >>= return . getOptions
                                  runX ( setTraceLevel 1 -- hxtSetTraceAndErrorLogger DEBUG -- NOTICE
                                         >>>
					 ( case updpkg of
					   Nothing -> arrIO0 (hayooCacher resume)
					   Just [] -> ( traceMsg 0 "fetching hackage rss feed of recent packages"
							>>>
							listA getRecentPackages
							>>>
							traceValue 0 (("updating recent packages: " ++) . show)
							>>>
							( isA (not . null) `guards` arrIO hayooPackageUpdate )
						      )
					   Just ps -> ( traceMsg 0 ("updating packages: " ++ show ps)
							>>>
							arrIO0 (hayooPackageUpdate ps)
						      )
					 )
                                         >>>
                                         traceMsg 0 (unwords ["writing cache into XML file", out])
                                         >>>
                                         xpickleDocument xpickle [(a_indent, v_1)] out
                                         >>>
                                         traceMsg 0 "writing cache finished"

                                       )
                                    >> return ()

-- ------------------------------------------------------------
