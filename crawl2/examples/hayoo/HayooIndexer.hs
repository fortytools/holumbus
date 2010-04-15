{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import qualified Data.Binary			as B
import           Data.Char
import		 Data.Function.Selector		( update, getS )
import           Data.Maybe

import		 Hayoo.FunctionInfo
import           Hayoo.Haddock
import		 Hayoo.IndexConfig
import           Hayoo.URIConfig

import		 Holumbus.Crawler
import		 Holumbus.Crawler.IndexerCore

import           Holumbus.Index.Documents       ( Documents(..)
                                                , emptyDocuments
                                                )
import		 Holumbus.Index.Inverted.PrefixMem

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import		 System.FilePath		( takeExtension )
import		 System.IO

import		 Text.XML.HXT.Arrow		hiding ( readDocument )
import           Text.XML.HXT.Arrow.XmlCache
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

type HayooIndexerState         	= IndexerState       Inverted Documents FunctionInfo
type HayooIndexerConfig        	= IndexCrawlerConfig Inverted Documents FunctionInfo

type HayooIndexerCrawlerState	= CrawlerState HayooIndexerState

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

data AppOpts			= AO
                                  { ao_output	:: String
				  , ao_help	:: Bool
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
                                  { ao_output	= ""
				  , ao_help		= False
				  , ao_resume	= Nothing
				  , ao_packages	= []
				  , ao_recent	= False
				  , ao_msg		= ""
				  , ao_crawlDoc	= (5, 5, 0)						-- max docs, max par docs, max threads
				  , ao_crawlSav	= (500, "./tmp/ix-")					-- save intervall and path
				  , ao_crawlLog	= (DEBUG, DEBUG)					-- log cache and hxt
				  , ao_crawlPar	= setDocAge (60 * 60 * 24 * 30) $			-- cache remains valid 1 month
                                                  [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
						  , (a_compress, v_1		)			-- cache files will be compressed
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
				  , Option "o"  ["output"]	(ReqArg ( \ f x -> x { ao_output   = f    }) 	  	"OUTPUT-FILE")	"output file, when name has .xml extension, then state output as XML else binary output of index"
				  , Option "p"  ["packages"]	(ReqArg ( \ l x -> x { ao_packages = pkgList l }) 	"PACKAGE-LIST")	"update a comma separated list of packages"
				  , Option "r"  ["resume"] 	(ReqArg ( \ s x -> x { ao_resume   = Just s}) 	  	"FILE")		"resume program with status file"
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
                                  , Option "d"  ["valid"]	(ReqArg ( setOption parseTime
                                                                          (\ x t -> x { ao_crawlPar = setDocAge t $
                                                                                                      ao_crawlPar x
                                                                                      }
                                                                          )
                                                                        )					 	"DURATION")	"validate cache for pages older than given time, format: 10sec, 5min, 20hours, 3days, 5weeks, 1month"

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
main2 opts			= runX
				  ( hxtSetTraceAndErrorLogger (snd . ao_crawlLog $ opts)
                                    >>>
				    action
                                    >>>
				    ( if xmlOutput
				      then writeXml
				      else writeBin
				    )
				  )
				  >> exitSuccess
    where
    action0			= arrIO0 (hayooIndexer opts)
    action
	| not . null $ packageList
				= traceMsg 0 ("indexing packages: " ++ unwords packageList)
				  >>>
				  action0

	| isJust resume		= traceMsg 0 "resume document indexing"
				  >>>
				  action0

	| otherwise		= traceMsg 0 ("indexing hayoo pages")
				  >>>
				  action0

    writeXml			= traceMsg 0 (unwords ["writing indexer state into XML file", out])
                                  >>>
                                  xpickleDocument xpickle [(a_indent, v_1)] out
				  >>>
				  constA ()
				  >>>
                                  traceMsg 0 "writing indexer finished"

    writeBin			= traceMsg 0 (unwords ["writing index into binary file", out])
                                  >>>
                                  ( arrIO $ \ s ->
				    do
				    B.encodeFile out (getS theResultAccu s)
				  )
                                  >>>
				  constA ()
				  >>>
                                  traceMsg 0 "writing index finished"

    resume			= ao_resume   opts
    packageList			= ao_packages opts
    out				= ao_output   opts
    xmlOutput			= takeExtension out == ".xml"

-- ------------------------------------------------------------
