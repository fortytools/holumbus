{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Codec.Compression.BZip ( compress, decompress )

import           Control.DeepSeq

import qualified Data.Binary                    as B
import           Data.Char
import           Data.Function.Selector
import           Data.Maybe

import           W3W.IndexConfig
import           W3W.IndexTypes
import           W3W.PageInfo

-- Change here for local / global fh-w Index
import           W3W.URIConfig
-- import           W3W.URIConfigLocal

import           Holumbus.Crawler
import           Holumbus.Crawler.CacheCore
import           Holumbus.Crawler.IndexerCore

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Text.XML.HXT.Core
import           Text.XML.HXT.Cache
import           Text.XML.HXT.HTTP()
import           Text.XML.HXT.Curl

import           qualified W3W.Date as D
import           Holumbus.Crawler.PdfToText
import           qualified Text.XML.HXT.DOM.XmlNode as XN

-- ------------------------------------------------------------
preDocumentFilter :: IOSArrow XmlTree XmlTree
preDocumentFilter = choiceA
        [ isHtmlContents  :-> this          -- do nothing
        , isPdfContents :-> extractPdfText  -- extract the text from a pdf
        , this    :-> replaceChildren none  -- throw away every contents
        ]
        where
            extractPdfText  = traceDoc "Indexer: extractPdfText: start"
                              >>>
                              processChildren ( deep getText >>> pdfToTextA >>> mkText )
                              >>>
                              traceDoc "Indexer: extractPdfText: result"
        
w3wIndexer                      :: AppOpts -> IO W3WIndexerCrawlerState
w3wIndexer o                    = stdIndexer
                                  config
                                  (ao_resume o)
                                  w3wStart
                                  emptyW3WState
    where
    config0                     = indexCrawlerConfig
                                  (ao_crawlPar o)
                                  w3wRefs
                                  Nothing
                                  (Just $ checkDocumentStatus >>> checkTransferStatus >>> preDocumentFilter)
                                  (Just $ w3wGetTitle)
                                  (Just $ w3wGetPageInfo dateExtractor dateProcessorContext)
                                  (w3wIndexConfig dateExtractor dateProcessorIndex)
                                  where
                                    dateExtractor               = D.extractDateRep
                                    dateProcessorIndex          = D.dateRep2NormalizedDates
                                    dateProcessorContext        = D.dateRep2DatesContext

    config                      = ao_crawlFct o $
                                  setCrawlerTraceLevel ct ht $
                                  setCrawlerSaveConf si sp   $
                                  setCrawlerMaxDocs md mp mt $
                                  config0

    (ct, ht)                    = ao_crawlLog o
    (si, sp)                    = ao_crawlSav o
    (md, mp, mt)                = ao_crawlDoc o
    checkTransferStatus         = ( getAttrValue transferStatus
                                    >>>
                                    isA (== "200")
                                  )
                                  `guards` this


-- ------------------------------------------------------------

w3wCacher                       :: AppOpts -> IO CacheCrawlerState
w3wCacher o                     = stdCacher
                                  (ao_crawlDoc o)
                                  (ao_crawlSav o)
                                  (ao_crawlLog o)
                                  (ao_crawlPar o)
                                  (ao_crawlCch o)
                                  (ao_resume o)
                                  w3wStart
                                  w3wRefs

-- ------------------------------------------------------------

data AppAction                  = BuildIx | BuildCache
                                  deriving (Eq, Show)

data AppOpts                    = AO
                                  { ao_index    :: String
                                  , ao_ixout    :: String
                                  , ao_ixsearch :: String
                                  , ao_xml      :: String
                                  , ao_help     :: Bool
                                  , ao_action   :: AppAction
                                  , ao_defrag   :: Bool
                                  , ao_resume   :: Maybe String
                                  , ao_msg      :: String
                                  , ao_crawlDoc :: (Int, Int, Int)
                                  , ao_crawlSav :: (Int, String)
                                  , ao_crawlLog :: (Priority, Priority)
                                  , ao_crawlPar :: SysConfig
                                  , ao_crawlFct :: W3WIndexerConfig    -> W3WIndexerConfig
                                  , ao_crawlCch :: CacheCrawlerConfig    -> CacheCrawlerConfig
                                  }

type SetAppOpt                  = AppOpts -> AppOpts

-- ------------------------------------------------------------

withCache'                      :: Int -> XIOSysState -> XIOSysState
withCache' sec                  = withCache "./cache" sec yes

initAppOpts                     :: AppOpts
initAppOpts                     = AO
                                  { ao_index    = ""
                                  , ao_ixout    = ""
                                  , ao_ixsearch = ""
                                  , ao_xml      = ""
                                  , ao_help     = False
                                  , ao_action   = BuildIx
                                  , ao_defrag   = False
                                  , ao_resume   = Nothing
                                  , ao_msg      = ""
                                  , ao_crawlDoc = (25000, 1024, 1)                                      -- max docs, max par docs, max threads: no parallel threads, but 1024 docs are indexed before results are inserted
                                  , ao_crawlSav = (1024, "./tmp/ix-")                                   -- save intervall and path
                                  , ao_crawlLog = (DEBUG, NOTICE)                                       -- log cache and hxt
                                  , ao_crawlPar = withCache' (60 * 60 * 24 * 1)                         -- set cache dir, cache remains valid 1 day, 404 pages are cached
                                                  >>>
                                                  withCompression (compress, decompress)                -- compress cache files
                                                  >>>
                                                  withStrictDeserialize yes                             -- strict input of cache files
                                                  >>>
                                                  withAcceptedMimeTypes [ text_html
                                                                        , application_xhtml
                                                                        , application_pdf
                                                                        ]
                                                  >>>
                                                  withCurl [ (curl_location,             v_1)            -- automatically follow redirects
                                                           , (curl_max_redirects,        "3")            -- but limit # of redirects to 3
                                                           ]
                                                  >>>
                                                  -- withHTTP [ (curl_max_redirects,        "3") ]          -- nice try: HTTP web access instead of curl, problem: no document size limit
                                                  -- >>>
                                                  withRedirect yes
                                                  >>>
                                                  withInputOption curl_max_filesize (show (1024 * 1024 * 3 `div` 2 ::Int)) -- this limit excludes automtically generated pages, sometimes > 1.5 Mbyte
                                                  >>>
                                                  withParseHTML no
                                                  >>>
                                                  withParseByMimeType yes

                                  , ao_crawlFct = ( editPackageURIs                                     -- configure URI rewriting
                                                    >>>
                                                    disableRobotsTxt                                    -- for w3w robots.txt is not needed
                                                  )
                                  , ao_crawlCch = ( editPackageURIs                                     -- configure URI rewriting
                                                    >>>
                                                    disableRobotsTxt                                    -- for w3w robots.txt is not needed
                                                  )
                                  }
    where
    editPackageURIs             = id -- chgS theProcessRefs (>>> arr rewriteURIs)


-- ------------------------------------------------------------

main                            :: IO ()
main                            = do
                                  pn   <- getProgName
                                  args <- getArgs
                                  main1 pn args

-- ------------------------------------------------------------

main1                           :: String -> [String] -> IO ()
main1 pn args
    | ao_help appOpts           = do
                                  hPutStrLn stderr (ao_msg appOpts ++ "\n" ++ usageInfo pn optDescr)
                                  if null (ao_msg appOpts)
                                     then exitSuccess
                                     else exitFailure
    | otherwise                 = do
                                  setLogLevel' "" (fst . ao_crawlLog $ appOpts)
                                  runX (  hxtSetTraceAndErrorLogger (snd . ao_crawlLog $ appOpts)
                                          >>>
                                          ( case ao_action appOpts of
                                            BuildCache -> mainCache
                                            BuildIx    -> mainIndex
                                          ) appOpts
                                       )
                                    >> exitSuccess
    where
    appOpts                     = foldl (.) (ef1 . ef2) opts $ initAppOpts
    (opts, ns, es)              = getOpt Permute optDescr args
    ef1
        | null es               = id
        | otherwise             = \ x -> x { ao_help   = True
                                           , ao_msg = concat es
                                           }
        | otherwise             = id
    ef2
        | null ns               = id
        | otherwise             = \ x -> x { ao_help   = True
                                           , ao_msg = "wrong program arguments: " ++ unwords ns
                                           }

    optDescr                    = [ Option "h?" ["help"]        (NoArg  $ \   x -> x { ao_help     = True })                            "usage info"
                                  , Option ""   ["build-index"] (NoArg  $ \   x -> x { ao_crawlSav = (500, "./tmp/ix-") })              "build W3W index (default)"
                                  , Option ""   ["build-cache"] (NoArg  $ \   x -> x { ao_action   = BuildCache })                      "update the W3W cache"

                                  , Option "i"  ["index"]       (ReqArg ( \ f x -> x { ao_index    = f    })            "INDEX")        "index input file (binary format) to be operated on"
                                  , Option "n"  ["new-index"]   (ReqArg ( \ f x -> x { ao_ixout    = f    })            "NEW-INDEX")    "new index file (binary format) to be generatet, default is index file"
                                  , Option "s"  ["new-search"]  (ReqArg ( \ f x -> x { ao_ixsearch = f    })            "SEARCH-INDEX") "new search index files (binary format) ready to be used by W3W! search"
                                  , Option "x"  ["xml-output"]  (ReqArg ( \ f x -> x { ao_xml      = f    })            "XML-FILE")     "output of final crawler state in xml format, \"-\" for stdout"
                                  , Option "r"  ["resume"]      (ReqArg ( \ s x -> x { ao_resume   = Just s})           "FILE")         "resume program with file containing saved intermediate state"

                                  -- , Option "u"  ["update"]      (NoArg  $ \   x -> x { ao_action   = UpdatePkg })                       "update packages specified by \"packages\" option"
                                  -- , Option "d"  ["delete"]      (NoArg  $ \   x -> x { ao_action   = RemovePkg })                       "delete packages specified by \"packages\" option"
                                  , Option ""   ["maxdocs"]     (ReqArg ( setOption parseInt
                                                                          (\ x i -> x { ao_crawlDoc = setMaxDocs i $
                                                                                                      ao_crawlDoc x
                                                                                      }
                                                                          )
                                                                        )                                               "NUMBER")       "maximum # of docs to be processed"
                                  , Option ""   ["maxthreads"]  (ReqArg ( setOption parseInt
                                                                          (\ x i -> x { ao_crawlDoc = setMaxThreads i $
                                                                                                      ao_crawlDoc x
                                                                                      }
                                                                          )
                                                                        )                                               "NUMBER")       "maximum # of parallel threads, 0: sequential, 1: single thread with binary merge, else real parallel threads, default: 1"
                                  , Option ""   ["maxpar"]      (ReqArg ( setOption parseInt
                                                                          (\ x i -> x { ao_crawlDoc = setMaxParDocs i $
                                                                                                      ao_crawlDoc x
                                                                                      }
                                                                          )
                                                                        )                                               "NUMBER")       "maximum # of docs indexed at once before the results are inserted into index, default: 1024"
                                  , Option ""   ["valid"]       (ReqArg ( setOption parseTime
                                                                          (\ x t -> x { ao_crawlPar = setDocAge t $
                                                                                                      ao_crawlPar x
                                                                                      }
                                                                          )
                                                                        )                                               "DURATION")     "validate cache for pages older than given time, format: 10sec, 5min, 20hours, 3days, 5weeks, 1month, default is 1month"
                                  , Option ""   ["defragment"]  (NoArg  $ \   x -> x { ao_defrag    = True  })                          "defragment index after delete or update"
                                  ]
    setOption parse f s x       = either (\ e -> x { ao_msg  = e
                                                   , ao_help = True
                                                   }
                                         ) (f x) . parse $ s

-- ------------------------------------------------------------

parseInt                                :: String -> Either String Int
parseInt s
    | match "[0-9]+" s                  = Right $ read s
    | otherwise                         = Left  $ "number expected in option arg"

parseTime                               :: String -> Either String Int
parseTime s
    | match "[0-9]+(s(ec)?)?"      s    = Right $ t
    | match "[0-9]+(m(in)?)?"      s    = Right $ t * 60
    | match "[0-9]+(h(our(s)?)?)?" s    = Right $ t * 60 * 60
    | match "[0-9]+(d(ay(s)?)?)?"  s    = Right $ t * 60 * 60 * 24
    | match "[0-9]+(w(eek(s)?)?)?" s    = Right $ t * 60 * 60 * 24 * 7
    | match "[0-9]+(m(onth(s)?)?)?" s   = Right $ t * 60 * 60 * 24 * 30
    | match "[0-9]+(y(ear(s)?)?)?" s    = Right $ t * 60 * 60 * 24 * 30 * 365
    | otherwise                         = Left  $ "error in duration format in option arg"
    where
    t                                   = read . filter isDigit $ s

-- ------------------------------------------------------------

setMaxDocs                              :: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxDocs    md (_md, mp, mt)          = (md, md `min` mp, mt)

setMaxParDocs                           :: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxParDocs mp (md, _mp, mt)          = (md, mp, mt)

setMaxThreads                           :: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxThreads mt (md, mp, _mt)          = (md, mp, mt)

setDocAge                               :: Int -> SysConfig -> SysConfig
setDocAge d                             = (>>> withCache' d)

-- ------------------------------------------------------------

mainIndex                       :: AppOpts -> IOSArrow b ()
mainIndex opts                  = action opts
                                  >>>
                                  writeSearchBin opts
                                  >>>
                                  writeResults opts
    where
    actIndex  opts'             = arrIO0 (w3wIndexer opts' >>= return . getS theResultAccu)

    action opts'
        | isJust resume         = traceMsg 0 "resume W3W document indexing"
                                  >>>
                                  actIndex opts'

        | otherwise             = traceMsg 0 "indexing all W3W pages"
                                  >>>
                                  actIndex opts'
        where
        resume                  = ao_resume   opts'

-- ------------------------------------------------------------

mainCache                       :: AppOpts -> IOSArrow b ()
mainCache opts                  = action opts
                                  >>>
                                  writeResults opts
    where
    actBuild opts'              = arrIO0 (w3wCacher opts')

    action opts'
        | isJust resume         = traceMsg 0 "resume W3W cache update"
                                  >>>
                                  actBuild opts'

        | otherwise             = traceMsg 0 ("cache W3W pages")
                                  >>>
                                  actBuild opts'
        where
        resume                  = ao_resume   opts'

-- ------------------------------------------------------------

rnfIO                           :: (ArrowIO a, NFData b) => a b b
rnfIO                           = arrIO (\ x -> rnf x `seq` return x)

-- ------------------------------------------------------------

writeResults                    :: (XmlPickler a, B.Binary a) => AppOpts -> IOSArrow a ()
writeResults opts               = writeXml opts
                                  >>>
                                  writeBin opts

-- ------------------------------------------------------------

writeXml                        :: (XmlPickler a) =>
                                   AppOpts -> IOSArrow a a
writeXml opts
    | xmlOut                    = traceMsg 0 (unwords ["writing index into XML file", xmlFile])
                                  >>>
                                  perform (xpickleDocument xpickle [withIndent yes] xmlFile)
                                  >>>
                                  traceMsg 0 "writing index finished"
    | otherwise                 = this
    where
    (xmlOut, xmlFile)
        | null xf               = (False, xf)
        | xf == "-"             = (True,  "")
        | otherwise             = (True,  xf)
        where
        xf                      = ao_xml opts

-- ------------------------------------------------------------

writeSearchBin                  :: (B.Binary a) =>
                                   AppOpts -> IOSArrow (W3WState a) (W3WState a)
writeSearchBin opts
    | null out                  = traceMsg 0 "no search index written"
    | otherwise                 = traceMsg 0 (unwords ["writing small document table into binary file", docFile])
                                  >>>
                                  perform ( (ixs_documents >>> docTable2smallDocTable)
                                            ^>>
                                            (arrIO $ B.encodeFile docFile)
                                          )
                                  >>>
                                  traceMsg 0 (unwords ["writing compressed inverted index into binary file", idxFile])
                                  >>>
                                  perform ( (ixs_index >>> inverted2compactInverted)
                                            ^>>
                                            (arrIO $ B.encodeFile idxFile)
                                          )
                                  >>>
                                  traceMsg 0 "writing search index files finished"
    where
    out                         = ao_ixsearch opts
    docFile                     = out ++ ".doc"
    idxFile                     = out ++ ".idx"

-- ------------------------------------------------------------

writeBin                        :: (B.Binary a) =>
                                   AppOpts -> IOSArrow a ()

writeBin opts
    | null out                  = traceMsg 0 "no binary result file written"
                                  >>>
                                  none
    | otherwise                 = traceMsg 0 (unwords ["writing index into binary file", out])
                                  >>>
                                  ( arrIO $ B.encodeFile out )
                                  >>>
                                  traceMsg 0 "writing index finished"
    where
    out
        | null oxf              = ao_index opts
        | otherwise             = oxf
        where
        oxf                     = ao_ixout opts

-- ------------------------------------------------------------
