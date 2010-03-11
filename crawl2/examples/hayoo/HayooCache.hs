{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import		 Data.Function.Selector		( update )
import           Data.Maybe

import		 Holumbus.Crawler
import		 Holumbus.Crawler.CacheCore

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import		 Text.XML.HXT.Arrow	hiding	( readDocument ) 
import           Text.XML.HXT.Arrow.XmlCache
import		 Text.XML.HXT.XPath

import		 HayooConfig

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

hayooCacher 			:: AppOpts -> IO CacheState
hayooCacher o              	= stdCacher (ao_crawlDoc o) (ao_crawlSav o) (ao_crawlLog o) (ao_crawlPar o) (ao_crawlFct o) (ao_resume o)
				            hayooStart
					    (hayooRefs [])

-- ------------------------------------------------------------

hayooPackageUpdate		:: AppOpts -> [String] -> IO CacheState
hayooPackageUpdate o pkgs	= stdCacher (ao_crawlDoc o) (ao_crawlSav o) (ao_crawlLog o) crawlPar' (ao_crawlFct o) Nothing
				            hayooStart
					    (hayooRefs pkgs)
    where
    crawlPar'			= addEntries [(a_document_age, show $ (1 * 1 * 1 * 1::Int))] (ao_crawlPar o)		-- cache validation initiated (1 sec valid) 

-- ------------------------------------------------------------

data AppOpts			= AO { ao_output	:: String
				     , ao_help		:: Bool
				     , ao_resume	:: Maybe String
				     , ao_packages	:: [String]
				     , ao_recent	:: Bool
				     , ao_msg		:: String
				     , ao_crawlDoc	:: (Int, Int, Int)
				     , ao_crawlSav	:: (Int, String)
				     , ao_crawlLog	:: (Priority, Priority)
				     , ao_crawlPar	:: [(String, String)]
				     , ao_crawlFct	:: CacheCrawlerConfig -> CacheCrawlerConfig
				     }

type SetAppOpt			= AppOpts -> AppOpts

-- ------------------------------------------------------------

initAppOpts			:: AppOpts
initAppOpts			= AO { ao_output	= ""
				     , ao_help		= False
				     , ao_resume	= Nothing
				     , ao_packages	= []
				     , ao_recent	= False
				     , ao_msg		= ""
				     , ao_crawlDoc	= (15000, 100, 10)					-- max docs, max par docs, max threads
				     , ao_crawlSav	= (500, "./tmp/ix-")					-- save intervall and path
				     , ao_crawlLog	= (DEBUG, NOTICE)					-- log cache and hxt
				     , ao_crawlPar	= [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
							  , (a_compress, v_1		)			-- cache files will be compressed
							  , (a_document_age,
							     show $ (60 * 60 * 24 * 1::Int))			-- cache remains valid 1 day
							  , (a_accept_mimetypes, 	unwords [text_html, application_xhtml])
							  , (a_parse_html,              v_0)
							  , (a_parse_by_mimetype,	v_1)
							  ]
				     , ao_crawlFct	= ( editPackageURIs					-- configure URI rewriting
							    >>>
							    disableRobotsTxt					-- for hayoo robots.txt is not needed
							  )
				     }

-- ------------------------------------------------------------

main				:: IO ()
main				= do
				  pn   <- getProgName
				  args <- getArgs
				  main1 pn args

-- ------------------------------------------------------------

main1				:: String -> [String] -> IO ()
main1 pn args
    | ao_help appOpts		= do
				  hPutStrLn stderr (ao_msg appOpts ++ "\n" ++ usageInfo pn optDescr)
				  if null (ao_msg appOpts)
				     then exitSuccess
				     else exitFailure
    | otherwise			= do
				  setLogLevel' "" (fst . ao_crawlLog $ appOpts)
				  main2 appOpts
    where
    appOpts			= foldl (.) (ef1 . ef2) opts $ initAppOpts
    (opts, ns, es)		= getOpt Permute optDescr args
    ef1
	| null es		= id
 	| otherwise		= \ x -> x { ao_help   = True
					   , ao_msg = concat es
					   }
	| otherwise		= id
    ef2
	| null ns		= id
	| otherwise		= \ x -> x { ao_help   = True
					   , ao_msg = "wrong program arguments: " ++ unwords ns
					   }

    optDescr			= [ Option "h?" ["help"] 	(NoArg  $ \   x -> x { ao_help     = True }) 				"usage info"
				  , Option "o"  ["output"]	(ReqArg ( \ f x -> x { ao_output   = f    }) 	  "OUTPUT-FILE")	"output file"
				  , Option "p"  ["packages"]	(ReqArg ( \ l x -> x { ao_packages = pkgList l }) "PACKAGE-LIST")	"update a comma separated list of packages"
				  , Option "r"  ["resume"] 	(ReqArg ( \ s x -> x { ao_resume   = Just s}) 	  "FILE")		"resume program with status file"
				  , Option "u"  ["update"]	(NoArg  $ \   x -> x { ao_recent   = True })				"update packages from hackage rss feed" 
				  ]
    pkgList			= words . map (\ x -> if x == ',' then ' ' else x)

-- ------------------------------------------------------------

main2                           :: AppOpts -> IO ()
main2 opts			= runX ( hxtSetTraceAndErrorLogger (snd . ao_crawlLog $ opts)
                                         >>>
				         action
                                         >>>
                                         traceMsg 0 (unwords ["writing cache into XML file", out])
                                         >>>
                                         xpickleDocument xpickle [(a_indent, v_1)] out
                                         >>>
                                         traceMsg 0 "writing cache finished"
				       )
				  >> exitSuccess
    where
    action
	| ao_recent opts	= traceMsg 0 "fetching hackage rss feed of recent packages"
				  >>>
				  listA getRecentPackages
				  >>>
				  traceValue 0 (("updating recent packages: " ++) . show)
				  >>>
				  ( isA (not . null)
				    `guards`
				    arrIO (hayooPackageUpdate opts)
				  )

	| not . null $ packageList
				= traceMsg 0 ("updating packages: " ++ unwords packageList)
				  >>>
				  arrIO0 (hayooPackageUpdate opts packageList)

	| isJust resume		= traceMsg 0 "resume document crawling"
				  >>>
				  arrIO0 (hayooCacher opts)

	| otherwise		= traceMsg 0 ("cache hayoo pages")
				  >>>
				  arrIO0 (hayooCacher opts)

    resume			= ao_resume   opts
    packageList			= ao_packages opts
    out				= ao_output   opts

-- ------------------------------------------------------------
