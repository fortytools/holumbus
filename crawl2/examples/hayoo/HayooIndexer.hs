{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Control.DeepSeq

import qualified Data.Binary			as B
import           Data.Char
import		 Data.Function.Selector		( update, getS )
import           Data.Maybe

import           Hayoo.HackagePackage
import           Hayoo.Haddock
import		 Hayoo.IndexConfig
import           Hayoo.IndexTypes
import           Hayoo.PackageArchive
import           Hayoo.URIConfig

import		 Holumbus.Crawler
import		 Holumbus.Crawler.CacheCore
import		 Holumbus.Crawler.IndexerCore

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import		 System.IO

import		 Text.XML.HXT.Arrow		hiding ( readDocument )
import           Text.XML.HXT.Arrow.XmlCache
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch


-- ------------------------------------------------------------

hayooIndexer			:: AppOpts -> IO HayooIndexerCrawlerState
hayooIndexer o                  = stdIndexer
                                  config
                                  (ao_resume o)
                                  hayooStart
                                  emptyHayooState
    where
    config0			= indexCrawlerConfig
                                  (ao_crawlPar o)
                                  (hayooRefs True $ ao_packages o)
                                  Nothing
                                  (Just $ checkDocumentStatus >>> prepareHaddock)
                                  (Just $ hayooGetTitle)
				  (Just $ hayooGetFctInfo)
                                  hayooIndexContextConfig

    config			= ao_crawlFct o $
                                  setCrawlerTraceLevel ct ht $
                                  setCrawlerSaveConf si sp   $
                                  setCrawlerMaxDocs md mp mt $
                                  config0

    (ct, ht)			= ao_crawlLog o
    (si, sp)			= ao_crawlSav o
    (md, mp, mt)		= ao_crawlDoc o

-- ------------------------------------------------------------

hayooPkgIndexer			:: AppOpts -> IO HayooPkgIndexerCrawlerState
hayooPkgIndexer o               = stdIndexer
                                  config
                                  (ao_resume o)
                                  hackageStart
                                  emptyHayooState
    where
    config0			= indexCrawlerConfig
                                  (ao_crawlPar o)
                                  (hayooRefs False $ ao_packages o)
                                  Nothing
                                  (Just $ checkDocumentStatus >>> preparePkg)
                                  (Just $ hayooGetPkgTitle)
				  (Just $ hayooGetPkgInfo)
                                  hayooPkgIndexContextConfig

    config			= ao_crawlPkg o $
                                  setCrawlerTraceLevel ct ht $
                                  setCrawlerSaveConf si sp   $
                                  setCrawlerMaxDocs md mp mt $
                                  config0

    (ct, ht)			= ao_crawlLog o
    (si, sp)			= ao_crawlSav o
    (md, mp, mt)		= ao_crawlDoc o

-- ------------------------------------------------------------

removePackages			:: (B.Binary di, NFData di) =>
                                   (Document di -> String) -> AppOpts -> IO (HayooState di)
removePackages getPkgName' o	= removePackages' getPkgName' (ao_index o) (ao_packages o) (ao_defrag o)

removePackagesIx		:: AppOpts -> IO HayooIndexerState 
removePackagesIx		= removePackages getPkgNameFct

removePackagesPkg		:: AppOpts -> IO HayooPkgIndexerState 
removePackagesPkg		= removePackages getPkgNamePkg

-- ------------------------------------------------------------

hayooCacher 			:: AppOpts -> IO CacheCrawlerState
hayooCacher o              	= stdCacher
                                  (ao_crawlDoc o)
                                  (ao_crawlSav o)
                                  (ao_crawlLog o)
                                  (ao_crawlPar o)
                                  (ao_crawlCch o)
                                  (ao_resume o)
				  hayooStart
				  (hayooRefs True [])

-- ------------------------------------------------------------

hayooPackageUpdate		:: AppOpts -> [String] -> IO CacheCrawlerState
hayooPackageUpdate o pkgs	= stdCacher
                                  (ao_crawlDoc o)
                                  (ao_crawlSav o)
                                  (ao_crawlLog o)
                                  (ao_crawlPar o)
                                  -- (setDocAge 1 (ao_crawlPar o))		-- cache validation initiated (1 sec valid) 
                                  (ao_crawlCch o)
                                  Nothing
				  hayooStart
				  (hayooRefs True pkgs)


-- ------------------------------------------------------------

data AppAction			= BuildIx | UpdatePkg | RemovePkg | BuildCache
                                  deriving (Eq, Show)

data AppOpts			= AO
                                  { ao_index	:: String
                                  , ao_ixout    :: String
                                  , ao_xml	:: String
				  , ao_help	:: Bool
                                  , ao_pkgIndex	:: Bool
                                  , ao_action	:: AppAction
                                  , ao_defrag	:: Bool
				  , ao_resume	:: Maybe String
				  , ao_packages	:: [String]
				  , ao_latest   :: Maybe Int
				  , ao_getHack  :: Bool
                                  , ao_pkgRank	:: Bool
				  , ao_msg	:: String
				  , ao_crawlDoc	:: (Int, Int, Int)
				  , ao_crawlSav	:: (Int, String)
				  , ao_crawlLog	:: (Priority, Priority)
				  , ao_crawlPar	:: [(String, String)]
				  , ao_crawlFct	:: HayooIndexerConfig    -> HayooIndexerConfig
                                  , ao_crawlPkg :: HayooPkgIndexerConfig -> HayooPkgIndexerConfig
				  , ao_crawlCch	:: CacheCrawlerConfig    -> CacheCrawlerConfig
				  }

type SetAppOpt			= AppOpts -> AppOpts

-- ------------------------------------------------------------

initAppOpts			:: AppOpts
initAppOpts			= AO
                                  { ao_index	= ""
                                  , ao_ixout    = ""
                                  , ao_xml	= ""
				  , ao_help	= False
                                  , ao_pkgIndex	= False
                                  , ao_action	= BuildIx
                                  , ao_defrag	= False
				  , ao_resume	= Nothing
				  , ao_packages	= []
				  , ao_latest   = Nothing
				  , ao_getHack  = False
                                  , ao_pkgRank	= False
				  , ao_msg	= ""
				  , ao_crawlDoc	= (15000, 100, 10)					-- max docs, max par docs, max threads
				  , ao_crawlSav	= (500, "./tmp/ix-")					-- save intervall and path
				  , ao_crawlLog	= (DEBUG, NOTICE)					-- log cache and hxt
				  , ao_crawlPar	= setDocAge (60 * 60 * 24 * 30) $			-- cache remains valid 1 month
                                                  [ (a_cache, 	  "./cache"	)			-- local cache dir "cache"
						  , (a_compress,  v_1		)			-- cache files will be compressed
                                                  , (a_cache_404, v_1		)			-- 404 pages will also be cached, these are wrong URLs in haddock docs
													-- and remain wrong over time
						  , (a_accept_mimetypes,
 	                                             unwords [ text_html
                                                             , application_xhtml
                                                             ])
						  , (a_parse_html,              v_0)
						  , (a_parse_by_mimetype,	v_1)
						  ]
				  , ao_crawlFct	= ( editPackageURIs					-- configure URI rewriting
						    >>>
						    disableRobotsTxt					-- for hayoo robots.txt is not needed
						  )
                                  , ao_crawlPkg	= disableRobotsTxt
				  , ao_crawlCch = ( editPackageURIs					-- configure URI rewriting
						    >>>
						    disableRobotsTxt					-- for hayoo robots.txt is not needed
						  )
				  }
    where
    editPackageURIs		= update theProcessRefs (>>> arr editLatestPackage)


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
				  runX (  hxtSetTraceAndErrorLogger (snd . ao_crawlLog $ appOpts)
                                          >>>
				          ( if ao_action appOpts == BuildCache
					    then mainCache
					    else
					    if ao_pkgIndex appOpts
                                            then mainHackage
                                            else mainHaddock
                                          ) appOpts
                                       )
                                    >> exitSuccess
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
                                  , Option ""   ["fct-index"]	(NoArg  $ \   x -> x { ao_pkgIndex = False
                                                                                     , ao_crawlSav = (500, "./tmp/ix-") })		"process index for haddock functions and types (default)"
                                  , Option ""   ["pkg-index"]	(NoArg  $ \   x -> x { ao_pkgIndex = True
                                                                                     , ao_crawlSav = (500, "./tmp/pkg-") })		"process index for hackage package description pages"
				  , Option ""   ["cache"]	(NoArg  $ \   x -> x { ao_action   = BuildCache })			"update the cache"

				  , Option "i"  ["index"]	(ReqArg ( \ f x -> x { ao_index    = f    }) 	  	"INDEX")	"index input file (binary format) to be operated on"
                                  , Option "n"  ["new-index"]	(ReqArg ( \ f x -> x { ao_ixout    = f    }) 	  	"NEW-INDEX")	"new index file (binary format) to be generatet, default is index file"
				  , Option "x"  ["xml-output"] 	(ReqArg ( \ f x -> x { ao_xml      = f    }) 	  	"XML-FILE")	"output of final crawler state in xml format, \"-\" for stdout"
				  , Option "r"  ["resume"] 	(ReqArg ( \ s x -> x { ao_resume   = Just s}) 	  	"FILE")		"resume program with file containing saved intermediate state"

				  , Option "p"  ["packages"]	(ReqArg ( \ l x -> x { ao_packages = pkgList l }) 	"PACKAGE-LIST")	"packages to be processed, a comma separated list of package names"
                                  , Option "u"  ["update"]	(NoArg  $ \   x -> x { ao_action   = UpdatePkg })			"update packages specified by \"packages\" option"
                                  , Option "d"  ["delete"]	(NoArg  $ \   x -> x { ao_action   = RemovePkg })			"delete packages specified by \"packages\" option"
                                  , Option "f"  ["defragment"]	(NoArg  $ \   x -> x { ao_defrag   = True  })				"defragment index after delete or update"
                                  , Option "m"  ["maxdocs"]     (ReqArg ( setOption parseInt
                                                                          (\ x i -> x { ao_crawlDoc = setMaxDocs i $
                                                                                                      ao_crawlDoc x
                                                                                      }
                                                                          )
                                                                        )					  	"NUMBER")	"maximum # of docs to be processed"
                                  , Option "t"  ["maxthreads"]  (ReqArg ( setOption parseInt
                                                                          (\ x i -> x { ao_crawlDoc = setMaxThreads i $
                                                                                                      ao_crawlDoc x
                                                                                      }
                                                                          )
                                                                        )					  	"NUMBER")	"maximum # of parallel threads (0: no threads at all)"
                                  , Option ""   ["valid"]	(ReqArg ( setOption parseTime
                                                                          (\ x t -> x { ao_crawlPar = setDocAge t $
                                                                                                      ao_crawlPar x
                                                                                      }
                                                                          )
                                                                        )					 	"DURATION")	"validate cache for pages older than given time, format: 10sec, 5min, 20hours, 3days, 5weeks, 1month, default is 1month"
                                  , Option ""   ["latest"]	(ReqArg ( setOption parseTime
                                                                          (\ x t -> x { ao_latest   = Just t })
                                                                        )					 	"DURATION")	"select latest packages newer than given time, format like in option \"valid\""
                                  , Option ""   ["hackage"]	(NoArg  $ \   x -> x { ao_getHack   = True })				"when processing latest packages, first update the package list from hackage"
                                  , Option ""   ["ranking"]	(NoArg  $ \   x -> x { ao_pkgRank   = True })				"when processing package index, compute package rank, default is no rank"

				  ]
    pkgList			= words . map (\ x -> if x == ',' then ' ' else x)
    setOption parse f s x       = either (\ e -> x { ao_msg  = e
                                                   , ao_help = True
                                                   }
                                         ) (f x) . parse $ s

-- ------------------------------------------------------------

parseInt				:: String -> Either String Int
parseInt s
    | match "[0-9]+" s			= Right $ read s
    | otherwise				= Left  $ "number expected in option arg"

parseTime				:: String -> Either String Int
parseTime s
    | match "[0-9]+(s(ec)?)?"      s	= Right $ t
    | match "[0-9]+(m(in)?)?"      s	= Right $ t * 60
    | match "[0-9]+(h(our(s)?)?)?" s	= Right $ t * 60 * 60
    | match "[0-9]+(d(ay(s)?)?)?"  s	= Right $ t * 60 * 60 * 24
    | match "[0-9]+(w(eek(s)?)?)?" s	= Right $ t * 60 * 60 * 24 * 7
    | match "[0-9]+(m(onth(s)?)?)?" s	= Right $ t * 60 * 60 * 24 * 30
    | match "[0-9]+(y(ear(s)?)?)?" s	= Right $ t * 60 * 60 * 24 * 30 * 365
    | otherwise				= Left  $ "error in duration format in option arg"
    where
    t 					= read . filter isDigit $ s

-- ------------------------------------------------------------

setMaxDocs				:: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxDocs md (_md, mp, mt)		= (md, md `min` mp, mt)

setMaxThreads				:: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxThreads mt (md, mp, _mt)		= (md, mp, mt)

setDocAge				:: Int -> [(String, String)] -> [(String, String)]
setDocAge d				=  addEntries [(a_document_age, show d)]

-- ------------------------------------------------------------

mainHackage                     :: AppOpts -> IOSArrow b ()
mainHackage opts		= action opts
                                  >>>
                                  writeResults opts
    where
    actIndex  opts'		= arrIO0 (hayooPkgIndexer opts' >>= return . getS theResultAccu)
                                  >>>
                                  actRank opts'


    actRank opts'
        | ao_pkgRank opts'	= traceMsg 0 "computing package ranks"
                                  >>>
                                  arrIO (\ x -> let y = packageRanking x in rnf y `seq` return y)
                                  >>>
                                  traceMsg 0 "package rank computation finished"
        | otherwise		= this

    actRemove opts'		= traceMsg 0 ("deleting packages " ++ unwords (ao_packages opts') ++ " from hackage package index" )
                                  >>>
                                  arrIO0 (removePackagesPkg opts')

    actMerge			= traceMsg 0 ("merging existing hackage package index with new packages")
                                  >>>
                                  arrIO (\ (new, old) -> unionIndexerStatesM old new)

    actNoOp			= traceMsg 0 ("no packages to be processed")
                                  >>>
                                  none

    action opts'
        | ( ixAction `elem` [UpdatePkg, RemovePkg] )
          &&
          null packageList	= actNoOp
    
        | ixAction == RemovePkg	= actRemove opts'

        | ixAction == UpdatePkg	= ( actIndex (opts' { ao_action  = BuildIx
                                                    , ao_pkgRank = False	-- delayed until merged
                                                    })
                                    &&&
                                    actRemove opts'
                                  )
                                  >>>
                                  actMerge
                                  >>>
                                  actRank opts'

	| notNullPackageList	= traceMsg 0 ("indexing hackage package descriptions for packages: " ++ unwords packageList)
				  >>>
				  actIndex opts'

	| isJust resume		= traceMsg 0 "resume hackage package description indexing"
				  >>>
				  actIndex opts'

	| otherwise		= traceMsg 0 "indexing all hackage package descriptions"
				  >>>
				  actIndex opts'
        where
        resume			= ao_resume   opts'
        ixAction		= ao_action   opts'
        packageList		= ao_packages opts'
        notNullPackageList	= not . null $ packageList

-- ------------------------------------------------------------

mainHaddock                     :: AppOpts -> IOSArrow b ()
mainHaddock opts		= action opts
                                  >>>
                                  writeResults opts
    where
    actIndex  opts'		= arrIO0 (hayooIndexer opts' >>= return . getS theResultAccu)

    actRemove opts'		= traceMsg 0 ("deleting packages " ++ unwords (ao_packages opts') ++ " from haddock index" )
                                  >>>
                                  arrIO0 (removePackagesIx opts')

    actUpdate _     []		= traceMsg 0 "no packages to be reindexed"
                                  >>>
                                  none
    actUpdate opts' pl		= traceMsg 0 ("updating index with packages: " ++ unwords pl)
				  >>>
				  ( actIndex  (opts' { ao_action = BuildIx
						     , ao_packages = pl
						     }
					      )
                                    &&&
                                    actRemove (opts' { ao_action = RemovePkg
						     , ao_packages = pl
						     }
					      )
                                  )
                                  >>>
                                  actMerge

    actMerge			= traceMsg 0 ("merging existing haddock index with new packages")
                                  >>>
                                  arrIO (\ (new, old) -> unionIndexerStatesM old new)

    actNoOp			= traceMsg 0 ("no packages to be processed")
                                  >>>
                                  none

    action opts'
	| isJust latest		= traceMsg 0 "reindex with latest packages"
				  >>>
				  ( actUpdate (opts' { ao_latest = Nothing })
				    $< arrIO0 (getNewPackages (ao_getHack opts') (fromJust latest))
				  )
	                          >>>
				  traceMsg 0 "reindex with latest packages finished"

        | ( ixAction `elem` [UpdatePkg, RemovePkg] )
          &&
          null packageList	= actNoOp
    
        | ixAction == RemovePkg	= actRemove opts'

        | ixAction == UpdatePkg	= ( actIndex (opts' { ao_action = BuildIx })
                                    &&&
                                    actRemove opts'
                                  )
                                  >>>
                                  actMerge

	| notNullPackageList	= traceMsg 0 ("indexing haddock for packages: " ++ unwords packageList)
				  >>>
				  actIndex opts'

	| isJust resume		= traceMsg 0 "resume haddock document indexing"
				  >>>
				  actIndex opts'

	| otherwise		= traceMsg 0 "indexing all hayoo haddock pages"
				  >>>
				  actIndex opts'
	where
	latest                  = ao_latest   opts'
	resume			= ao_resume   opts'
	ixAction		= ao_action   opts'
	packageList		= ao_packages opts'
	notNullPackageList	= not . null $ packageList

-- ------------------------------------------------------------

mainCache                       :: AppOpts -> IOSArrow b ()
mainCache opts			= action opts
				  >>>
				  writeResults opts
    where
    actBuild opts'		= arrIO0 (hayooCacher opts')

    actUpdate _ []		= traceMsg 0 ("no packages to be updated")
				  >>>
				  none
    actUpdate opts' pl		= traceMsg 0 ("updating cache with packages: " ++ unwords pl)
				  >>>
				  arrIO0 (hayooPackageUpdate opts' pl)
    action opts'
	| isJust latest		= traceMsg 0 ("compute list of latest packages")
                                  >>>
                                  ( actUpdate (opts' { ao_latest   = Nothing
						     , ao_crawlPar = setDocAge 1 $ ao_crawlPar opts'	-- force cache update
						     }
					      )
				    $< arrIO0 (getNewPackages (ao_getHack opts') (fromJust latest))
				  )
				  >>>
                                  traceMsg 0 "update of latest packages finished"
				  
	| not . null $ packageList
				= traceMsg 0 ("updating cache with packages: " ++ unwords packageList)
				  >>>
				  actUpdate opts' packageList

	| isJust resume		= traceMsg 0 "resume cache update"
				  >>>
				  actBuild opts'

	| otherwise		= traceMsg 0 ("cache hayoo pages")
				  >>>
				  actBuild opts'
	where
	resume			= ao_resume   opts'
	packageList		= ao_packages opts'
	latest			= ao_latest   opts'

-- ------------------------------------------------------------

writeResults			:: (XmlPickler a, B.Binary a) => AppOpts -> IOSArrow a ()
writeResults opts		= writeXml opts
                                  >>>
                                  writeBin opts

-- ------------------------------------------------------------

writeXml			:: (XmlPickler a) =>
                                   AppOpts -> IOSArrow a a
writeXml opts
    | xmlOut			= traceMsg 0 (unwords ["writing index into XML file", xmlFile])
                                  >>>
                                  perform (xpickleDocument xpickle [(a_indent, v_1)] xmlFile)
				  >>>
                                  traceMsg 0 "writing index finished"
    | otherwise			= this
    where
    (xmlOut, xmlFile)
        | null xf		= (False, xf)
        | xf == "-"		= (True,  "")
        | otherwise		= (True,  xf)
        where
        xf			= ao_xml opts

-- ------------------------------------------------------------

writeBin			:: (B.Binary a) =>
                                   AppOpts -> IOSArrow a ()

writeBin opts
    | null out			= traceMsg 0 "no binary result file written"
                                  >>>
                                  none
    | otherwise			= traceMsg 0 (unwords ["writing index into binary file", out])
                                  >>>
                                  ( arrIO $ B.encodeFile out )
                                  >>>
                                  traceMsg 0 "writing index finished"
    where
    out
        | null oxf		= ao_index opts
        | otherwise		= oxf
        where
        oxf			= ao_ixout opts

-- ------------------------------------------------------------
