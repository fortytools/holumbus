{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Control.DeepSeq

import qualified Data.Binary			as B
import           Data.Char
import		 Data.Function.Selector		( update, getS )
import           Data.Maybe

import           Hayoo.Haddock
import		 Hayoo.IndexConfig
import           Hayoo.IndexTypes
import           Hayoo.URIConfig

import		 Holumbus.Crawler
import		 Holumbus.Crawler.IndexerCore

import           Holumbus.Index.Documents       ( emptyDocuments )

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
                                  ( emptyIndexerState emptyInverted emptyDocuments )
    where
    config0			= indexCrawlerConfig
                                  (ao_crawlPar o)
                                  (hayooRefs $ ao_packages o)
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

removePackages			:: AppOpts -> IO HayooIndexerState
removePackages o		= do
                                  ix <- B.decodeFile (ao_index o)
                                  let ix1  = removePack (ao_packages o) ix
                                  let ix2  = if ao_defrag o
	                                     then defragmentIndex ix1
                                             else ix1
                                  rnf ix2 `seq` return ix2

-- ------------------------------------------------------------

data AppAction			= BuildIx | UpdatePkg | RemovePkg
                                  deriving (Eq, Show)

data AppOpts			= AO
                                  { ao_index	:: String
                                  , ao_xml	:: String
				  , ao_help	:: Bool
                                  , ao_action	:: AppAction
                                  , ao_defrag	:: Bool
				  , ao_resume	:: Maybe String
				  , ao_packages	:: [String]
				  , ao_recent	:: Bool
				  , ao_msg	:: String
				  , ao_crawlDoc	:: (Int, Int, Int)
				  , ao_crawlSav	:: (Int, String)
				  , ao_crawlLog	:: (Priority, Priority)
				  , ao_crawlPar	:: [(String, String)]
				  , ao_crawlFct	:: HayooIndexerConfig -> HayooIndexerConfig
				  }

type SetAppOpt			= AppOpts -> AppOpts

-- ------------------------------------------------------------

initAppOpts			:: AppOpts
initAppOpts			= AO
                                  { ao_index	= ""
                                  , ao_xml	= ""
				  , ao_help	= False
                                  , ao_action	= BuildIx
                                  , ao_defrag	= False
				  , ao_resume	= Nothing
				  , ao_packages	= []
				  , ao_recent	= False
				  , ao_msg		= ""
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
				  , Option "i"  ["index"]	(ReqArg ( \ f x -> x { ao_index    = f    }) 	  	"INDEX-FILE")	"index file (binary format) to be generated or updated"
				  , Option "x"  ["xml-output"] 	(ReqArg ( \ f x -> x { ao_xml      = f    }) 	  	"XML-FILE")	"output of final crawler state in xml format, ( \"-\" for stdout"
				  , Option "r"  ["resume"] 	(ReqArg ( \ s x -> x { ao_resume   = Just s}) 	  	"FILE")		"resume program with status file"
				  , Option "p"  ["packages"]	(ReqArg ( \ l x -> x { ao_packages = pkgList l }) 	"PACKAGE-LIST")	"packages to be processed, a comma separated list of package names"
                                  , Option "u"  ["update"]	(NoArg  $ \   x -> x { ao_action   = UpdatePkg })			"update packages specified by \"packages\" option"
                                  , Option "d"  ["delete"]	(NoArg  $ \   x -> x { ao_action   = RemovePkg })			"delete packages specified by \"packages\" option"
                                  , Option "f"  ["defragment"]	(NoArg  $ \   x -> x { ao_defrag   = True })				"defragment index after delete or update"
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
    | match "[0-9]+(w(wwk(s)?)?)?" s	= Right $ t * 60 * 60 * 24 * 7
    | match "[0-9]+(m(onth(s)?)?)?" s	= Right $ t * 60 * 60 * 24 * 30
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

main2                           :: AppOpts -> IO ()
main2 opts
    | otherwise			= runX
				  ( hxtSetTraceAndErrorLogger (snd . ao_crawlLog $ opts)
                                    >>>
				    action
                                    >>>
                                    writeXml opts
                                    >>>
                                    writeBin opts
				  )
				  >> exitSuccess
    where
    actIndex  opts'		= arrIO0 (hayooIndexer opts' >>= return . getS theResultAccu)

    actRemove opts'		= traceMsg 0 ("deleting packages " ++ unwords (ao_packages opts') ++ " from index" )
                                  >>>
                                  arrIO0 (removePackages opts')

    actMerge			= traceMsg 0 ("merging existing index with new packages")
                                  >>>
                                  arrIO (\ (new, old) -> unionIndexerStatesM old new)

    actNoOp			= traceMsg 0 ("no packages to be processed")
                                  >>>
                                  none
    action
        | ( ixAction `elem` [UpdatePkg, RemovePkg] )
          &&
          null packageList	= actNoOp
    
        | ixAction == RemovePkg	= actRemove opts

        | ixAction == UpdatePkg	= ( actIndex (opts { ao_action = BuildIx })
                                    &&&
                                    actRemove opts
                                  )
                                  >>>
                                  actMerge

	| notNullPackageList	= traceMsg 0 ("indexing packages: " ++ unwords packageList)
				  >>>
				  actIndex opts

	| isJust resume		= traceMsg 0 "resume document indexing"
				  >>>
				  actIndex opts

	| otherwise		= traceMsg 0 "indexing all hayoo pages"
				  >>>
				  actIndex opts

    resume			= ao_resume   opts
    ixAction			= ao_action   opts
    packageList			= ao_packages opts
    notNullPackageList		= not . null $ packageList

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

writeBin			:: (B.Binary b) =>
                                   AppOpts -> IOSArrow b ()

writeBin opts			= traceMsg 0 (unwords ["writing index into binary file", out])
                                  >>>
                                  ( arrIO $ B.encodeFile out )
                                  >>>
                                  traceMsg 0 "writing index finished"
        where
        out			= ao_index    opts

-- ------------------------------------------------------------
