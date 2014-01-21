{-# LANGUAGE FlexibleContexts #-}

-- ------------------------------------------------------------

module Main (main)
where

import           Codec.Compression.BZip     (compress, decompress)

import           Control.DeepSeq
-- import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader

import qualified Data.Binary                as B
import           Data.Char
import           Data.Function.Selector
import           Data.Maybe
import           Data.Size
import           Data.Typeable

import qualified Hayoo.FctIndexerCore       as FJ
import           Hayoo.HackagePackage
import           Hayoo.Haddock
import           Hayoo.IndexConfig
import           Hayoo.IndexerCore
import           Hayoo.IndexTypes
import           Hayoo.PackageArchive
import qualified Hayoo.PkgIndexerCore       as PJ
import           Hayoo.PostToServer
import           Hayoo.URIConfig

import           Holumbus.Crawler
import           Holumbus.Crawler.CacheCore

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Text.XML.HXT.Cache
import           Text.XML.HXT.Core
import           Text.XML.HXT.Curl
import           Text.XML.HXT.HTTP          ()

-- ------------------------------------------------------------

data AppAction
    = BuildIx | UpdatePkg | RemovePkg | BuildCache | MergeIx
      deriving (Eq, Show)

data AppOpts
    = AO
      { ao_progname    :: String
      , ao_index       :: String
      , ao_ixout       :: String
      , ao_ixsearch    :: String
      , ao_xml         :: String
      , ao_help        :: Bool
      , ao_pkgIndex    :: Bool
      , ao_JSON        :: Bool
      , ao_JSONserv    :: Maybe String
      , ao_action      :: AppAction
      , ao_defrag      :: Bool
      , ao_partix      :: Bool
      , ao_resume      :: Maybe String
      , ao_packages    :: [String]
      , ao_latest      :: Maybe Int
      , ao_getHack     :: Bool
      , ao_pkgRank     :: Bool
      , ao_pkgRankOnly :: Bool
      , ao_msg         :: String
      , ao_crawlDoc    :: (Int, Int, Int)
      , ao_crawlSav    :: Int
      , ao_crawlSfn    :: String
      , ao_crawlLog    :: (Priority, Priority)
      , ao_crawlPar    :: SysConfig
      , ao_crawlFct    :: HayooIndexerConfig    -> HayooIndexerConfig
      , ao_crawlPkg    :: HayooPkgIndexerConfig -> HayooPkgIndexerConfig
      , ao_crawlCch    :: CacheCrawlerConfig    -> CacheCrawlerConfig
      , ao_crawlPkJ    :: PJ.PkgCrawlerConfig   -> PJ.PkgCrawlerConfig
      , ao_crawlFcJ    :: FJ.FctCrawlerConfig   -> FJ.FctCrawlerConfig
      }

-- ------------------------------------------------------------

initAppOpts :: AppOpts
initAppOpts
    = AO
      { ao_progname     = "hayooCrawler"
      , ao_index        = ""
      , ao_ixout        = ""
      , ao_ixsearch     = ""
      , ao_xml          = ""
      , ao_help         = False
      , ao_pkgIndex     = False
      , ao_JSON         = False
      , ao_JSONserv     = Nothing
      , ao_action       = BuildIx
      , ao_defrag       = False
      , ao_partix       = False
      , ao_resume       = Nothing
      , ao_packages     = []
      , ao_latest       = Nothing
      , ao_getHack      = False
      , ao_pkgRank      = False
      , ao_pkgRankOnly  = False
      , ao_msg          = ""
      , ao_crawlDoc     = (50000, 1024, 1)                                          -- max docs, max par docs, max threads: no parallel threads, but 1024 docs are indexed before results are inserted
      , ao_crawlSav     = 5000                                                      -- save intervall
      , ao_crawlSfn     = "./tmp/ix-"                                               -- save path
      , ao_crawlLog     = (DEBUG, NOTICE)                                           -- log cache and hxt
      , ao_crawlPar     = withCache' (60 * 60 * 24 * 30)                            -- set cache dir, cache remains valid 1 month, 404 pages are cached
                          >>>
                          withCompression (compress, decompress)                    -- compress cache files
                          >>>
                          withStrictDeserialize yes                                 -- strict input of cache files
                          >>>
                          withAcceptedMimeTypes [ text_html
                                                , application_xhtml
                                                ]
                          >>>
                          withCurl [ (curl_location,             v_1)               -- automatically follow redirects
                                   , (curl_max_redirects,        "3")               -- but limit # of redirects to 3
                                   ]
                          >>>
                          -- withHTTP [ (curl_max_redirects,        "3") ]          -- nice try: HTTP web access instead of curl, problem: no document size limit
                          -- >>>
                          withRedirect yes
                          >>>
                          withInputOption curl_max_filesize
                                              (show (1024 * 1024 * 3 `div` 2 ::Int)) -- this limit excludes automtically generated pages, sometimes > 1.5 Mbyte
                          >>>
                          withParseHTML no
                          >>>
                          withParseByMimeType yes

      , ao_crawlFct     = ( editPackageURIs                                         -- configure URI rewriting
                            >>>
                            disableRobotsTxt                                        -- for hayoo robots.txt is not needed
                          )

      , ao_crawlPkg     = disableRobotsTxt

      , ao_crawlCch     = ( editPackageURIs                                         -- configure URI rewriting
                            >>>
                            disableRobotsTxt                                        -- for hayoo robots.txt is not needed
                          )

      , ao_crawlPkJ     = disableRobotsTxt

      , ao_crawlFcJ     = ( editPackageURIs                                         -- configure URI rewriting
                            >>>
                            disableRobotsTxt                                        -- for hayoo robots.txt is not needed
                          )
      }
    where
      editPackageURIs
          = chgS theProcessRefs (>>> arr editLatestPackage)

withCache' :: Int -> XIOSysState -> XIOSysState
withCache' sec
    = withCache "./cache" sec yes

-- ------------------------------------------------------------

type HIO = ReaderT AppOpts (ErrorT String IO)

main :: IO ()
main
    = do pn   <- getProgName
         args <- getArgs
         res  <- runErrorT $ runReaderT main2 (evalOptions pn args)
         either (const exitFailure) (const exitSuccess) res

-- ------------------------------------------------------------

main2 :: HIO ()
main2
    = do (h, pn) <- asks (ao_help &&& ao_progname)
         if h
            then do msg <- asks ao_msg
                    liftIO $ do hPutStrLn stderr (msg ++ "\n" ++ usageInfo pn hayooOptDescr)
                    if null msg
                       then return ()
                       else throwError "wrong option"
            else do asks (snd . ao_crawlLog) >>= setLogLevel ""
                    a <- asks ao_action
                    case a of
                      BuildCache -> mainCache
                      MergeIx    -> mainHaddock
                      _          -> do p <- asks ao_pkgIndex
                                       if p
                                          then mainHackage
                                          else mainHaddock

-- ------------------------------------------------------------

mainCache :: HIO ()
mainCache
    = do action
    where
      action
          = asks ao_latest >>=
            maybe action2 updatePackages
      action2
          = do pl <- asks ao_packages
               case pl of
                 [] -> action3
                 _  -> updatePkg pl

      action3
          = do rs <- asks ao_resume
               notice $ if isJust rs
                        then ["resume cache update"]
                        else ["cache hayoo pages"]
               hayooCacher >>= writeResults

      updatePackages latest
          = do notice ["compute list of latest packages"]
               (hk, cp) <- asks (ao_getHack &&& ao_crawlPar)
               pl       <- liftIO $ getNewPackages hk latest
               local (\ opts -> opts { ao_latest   = Nothing
                                     , ao_crawlPar = setDocAge 1 cp    -- force cache update
                                     }
                     ) $ updatePkg pl

      updatePkg []
          = notice ["no packages to be updated"]
      updatePkg ps
          = do notice $ "updating cache with packages:" : ps
               res  <- hayooPackageUpdate ps
               writeResults res
               notice $ "updating cache with latest packages done" : []

-- ------------------------------------------------------------

mainHackage :: HIO ()
mainHackage
    = action
    where
      action
          = do apl <- asks (ao_action &&& ao_packages)
               case apl of
                 (RemovePkg, [])
                     -> noaction
                 (RemovePkg, ps)
                     -> removePkg ps >>= writeRes
                 (UpdatePkg, [])
                     -> noaction
                 (UpdatePkg, ps)
                     -> updatePkg ps >>= writeRes
                 (_,         ps)
                     -> do rs <- asks ao_resume
                           if isJust rs
                              then notice ["resume hackage package description indexing"]
                              else return ()
                           js <- asks ao_JSON
                           if not js
                              then (indexPkg ps >>= writeRes)
                              else indexPkgJSON ps

      removePkg :: [String] -> HIO HayooPkgIndexerState
      removePkg ps
          = do notice $ "removing packages from hackage package index:" : ps
               res <- removePackagesPkg
               rnf res `seq`
                   notice $ "packages removed from hackage package index : " : ps
               return res

      updatePkg :: [String] -> HIO HayooPkgIndexerState
      updatePkg ps
          = do notice $ "updating packages from hackage package index:" : ps
               newix <- local (\ opts -> opts { ao_action  = BuildIx
                                              , ao_pkgRank = False
                                              }
                              ) (indexPkg ps)
               oldix <- removePkg ps
               mergePkg newix oldix

      indexPkg :: [String] -> HIO HayooPkgIndexerState
      indexPkg ps
          = do notice $ if null ps
                          then ["indexing all packages from hackage package index"]
                          else "indexing hackage package descriptions for packages:" : ps
               (getS theResultAccu `fmap` hayooPkgIndexer)
                        >>= rankPkg

      indexPkgJSON :: [String] -> HIO () -- HayooPkgIndexerState
      indexPkgJSON ps
          = do notice $ if null ps
                          then ["JSON format indexing all packages from hackage package index"]
                          else "JSON format indexing hackage package descriptions for packages:" : ps
               (getS theResultAccu `fmap` hayooPJIndexer) >>= (rankPkgJSON . ixs_documents)

      rankPkg ix
          = do rank <- asks ao_pkgRank
               if rank
                  then do notice ["computing package ranks"]
                          let res = packageRanking ix
                          rnf res `seq`
                              notice ["package rank computation finished"]
                          return res
                  else do notice ["no package ranks computed"]
                          return ix

      rankPkgJSON :: Documents PackageInfo -> HIO ()
      rankPkgJSON dt
          = do rank <- asks ao_pkgRank
               serv <- asks ao_JSONserv
               if rank
                  then do notice ["computing package ranks"]
                          liftIO $ (case serv of
                                      Nothing -> PJ.flushRanksToFile
                                      Just u  -> PJ.flushRanksToServer u
                                   ) (packageDocRanking dt)
                          notice ["JSON package ranks stored in file 'packages/0000-ranks.json'"]
                  else do notice ["no package ranks computed"]

mainHaddock :: HIO ()
mainHaddock
    = do action
    where
      action
          = do latest <- asks ao_latest
               maybe action2 updateLatest latest
      action2
          = do apl <- asks (ao_action &&& ao_packages)
               case apl of
                 (RemovePkg, [])
                     -> noaction
                 (RemovePkg, ps)
                     -> removePkg ps >>= writeRes
                 (UpdatePkg, [])
                     -> noaction
                 (UpdatePkg, ps)
                     -> updatePkg ps >>= writeRes
                 (MergeIx, _)
                     -> loadPartialIx >>= mergeAndWritePartialRes
                 (_,         ps)
                     -> do rs <- asks ao_resume
                           if isJust rs
                              then notice ["resume haddock document indexing"]
                              else return ()
                           js <- asks ao_JSON
                           if not js
                              then (indexPkg ps >>= writePartialRes)
                              else indexPkgJSON ps

      removePkg ps
          = do notice $ "deleting packages" : ps ++ ["from haddock index"]
               res <- removePackagesIx
               rnf res `seq`
                   notice $ "packages " : ps ++ ["deleted from haddock index"]
               return res

      updatePkg :: [String] -> HIO (HayooIndexerState)
      updatePkg ps
          = do notice $ "updating haddock index with packages:" : ps
               newix <- local (\ opts -> opts { ao_action  = BuildIx
                                              }
                              ) (fst `fmap` indexPkg ps)
               oldix <- removePkg ps
               mergePkg newix oldix

      loadPartialIx :: HIO [Int]
      loadPartialIx
          = local (\ o -> o { ao_action   = BuildIx
                            , ao_packages = []
                            }) (snd `fmap` indexPkg [])

      indexPkg :: [String] -> HIO (HayooIndexerState, [Int])
      indexPkg ps
          = do notice $ if null ps
                          then ["indexing all haddock pages"]
                          else "indexing haddock for packages:" : ps
               (getS (theResultAccu .&&&. theListOfDocsSaved) `fmap` hayooIndexer)

      indexPkgJSON :: [String] -> HIO ()
      indexPkgJSON ps
          = do notice $ if null ps
                          then ["JSON format indexing all haddock pages"]
                          else "JSON format indexing haddock pages:" : ps
               hayooFJIndexer >> return ()

      updateLatest latest
          = do notice ["reindex with latest packages"]
               hk  <- asks ao_getHack
               ps  <- liftIO $ getNewPackages hk latest
               if null ps
                  then notice ["no new packages to be indexed"]
                  else do res <- local (\ o -> o { ao_latest = Nothing }
                                       ) $ updatePkg ps
                          notice ["reindex with latest packages finished"]
                          writeRes res

-- ------------------------------------------------------------

noaction :: HIO ()
noaction
    = notice ["no packages to be processed"]

-- ------------------------------------------------------------

removePacks :: (B.Binary di, NFData di) =>
                   (Document di -> String) -> HIO (HolumbusState di)
removePacks getPkgName'
    = do (ix, (pkg, dfg)) <- asks (ao_index &&& ao_packages &&& ao_defrag)
         liftIO $ removePackages' getPkgName' ix pkg dfg

removePackagesIx ::HIO HayooIndexerState
removePackagesIx
    = removePacks getPkgNameFct

removePackagesPkg :: HIO HayooPkgIndexerState
removePackagesPkg
    = removePacks getPkgNamePkg

-- ------------------------------------------------------------

mergePkg :: (B.Binary a) => HolumbusState a -> HolumbusState a -> HIO (HolumbusState a)
mergePkg nix oix
    = do notice $ ["merging existing index with new packages"]
         liftIO $ unionIndexerStatesM oix nix

-- ------------------------------------------------------------
{-
logStats :: Sizeable a => String -> a -> HIO ()
logStats msg x
    = notice $ [msg, "\n\n" ++ show (statsOf x)]

logIndexerState :: (Sizeable i, Sizeable (d c)) => IndexerState i d c -> HIO ()
logIndexerState ixs
    = do logStats "space statistics for index"     (ixs_index     ixs)
         logStats "space statistics for documents" (ixs_documents ixs)
-- -}
-- ------------------------------------------------------------

writePartialRes :: (HayooIndexerState, [Int]) -> HIO ()
writePartialRes (x, ps)
    = do part <- asks ao_partix
         if part
            then mergeAndWritePartialRes ps
            else writeRes x

mergeAndWritePartialRes :: [Int] -> HIO ()
mergeAndWritePartialRes ps
    = do pxs <- (\ fn -> map (mkTmpFile 10 fn) ps) `fmap` asks ao_crawlSfn
         out <- asks ao_ixsearch
         mergeAndWritePartialRes' id' pxs out
    where
      id' :: SmallDocuments FunctionInfo -> SmallDocuments FunctionInfo
      id' = id

-- ------------------------------------------------------------

writeRes :: (XmlPickler a, B.Binary a, Sizeable a, Typeable a) => HolumbusState a -> HIO ()
writeRes x
    = writeSearchBin' x >> writeResults x
    where
      writeSearchBin' s
          = do out <- asks ao_ixsearch
               writeSearchBin out s

writeResults :: (XmlPickler a, B.Binary a) => a -> HIO ()
writeResults v
    = do (xf, of') <- asks (ao_xml &&& (ao_ixout &&& ao_index))
         writeXml xf  v
         writeBin (out of') v
    where
      out (bf, bi)
          | null bf     = bi
          | otherwise   = bf

-- ------------------------------------------------------------

hayooCacher :: HIO CacheCrawlerState
hayooCacher
    = do o <- ask
         liftIOC $ stdCacher
                    (ao_crawlDoc o)
                    (ao_crawlSav o, ao_crawlSfn o)
                    (ao_crawlLog o)
                    (ao_crawlPar o)
                    (ao_crawlCch o)
                    (ao_resume o)
                    hayooStart
                    (hayooRefs True [])

liftIOC :: IO (Either String a) -> HIO a
liftIOC action
    = liftIO action >>= either throwError return

-- ------------------------------------------------------------

hayooPackageUpdate :: [String] -> HIO CacheCrawlerState
hayooPackageUpdate pkgs
    = do o <- ask
         liftIOC $ stdCacher
                    (ao_crawlDoc o)
                    (ao_crawlSav o, ao_crawlSfn o)
                    (ao_crawlLog o)
                    (ao_crawlPar o)
                    -- (setDocAge 1 (ao_crawlPar o))              -- cache validation initiated (1 sec valid)
                    (ao_crawlCch o)
                    Nothing
                    hayooStart
                    (hayooRefs True pkgs)

-- ------------------------------------------------------------

hayooPkgIndexer :: HIO HayooPkgIndexerCrawlerState
hayooPkgIndexer
    = do o <- ask
         liftIOC $ stdIndexer
                    (config o)
                    (ao_resume o)
                    hackageStart
                    emptyHolumbusState
    where
    config0 o
        = indexCrawlerConfig
          (ao_crawlPar o)
          (hayooRefs False $ ao_packages o)
          Nothing
          (Just $ checkDocumentStatus >>> preparePkg)
          (Just $ hayooGetPkgTitle)
          (Just $ hayooGetPkgInfo)
          hayooPkgIndexContextConfig

    config o
        = ao_crawlPkg o $
          setCrawlerTraceLevel ct ht   $
          setCrawlerSaveConf si sp     $
          setCrawlerMaxDocs md mp mt   $
          config0                      $ o
        where
          (ct, ht)      = ao_crawlLog o
          si            = ao_crawlSav o
          sp            = ao_crawlSfn o
          (md, mp, mt)  = ao_crawlDoc o

-- ------------------------------------------------------------
--
-- the JSON package indexer

hayooPJIndexer :: HIO PJ.PkgCrawlerState
hayooPJIndexer
    = do o <- ask
         liftIOC $ stdIndexer
                    (config o)
                    (ao_resume o)
                    hackageStart
                    PJ.emptyPkgState
    where
      config0 o
          = PJ.indexCrawlerConfig
            flush
            (ao_crawlPar o)
            (hayooRefs False $ ao_packages o)
            Nothing
            (Just $ checkDocumentStatus >>> preparePkg)
            (Just $ hayooGetPkgTitle)
            (Just $ hayooGetPkgInfo)
            hayooPkgIndexContextConfig
          where
            flush
                = case (ao_pkgRankOnly o, ao_JSONserv o) of
                    (True, _      ) -> PJ.flushToDevNull
                    (_,    Nothing) -> PJ.flushToFile
                    (_,    Just u ) -> PJ.flushToServer u

      config o
          = ao_crawlPkJ o $
            setCrawlerTraceLevel ct ht   $
            setCrawlerSaveConf si sp     $
            setCrawlerMaxDocs md mp mt   $
            config0                      $ o
          where
            (ct, ht)      = ao_crawlLog o
            si            = ao_crawlSav o
            sp            = ao_crawlSfn o
            (md, mp, mt)  = ao_crawlDoc o

-- ------------------------------------------------------------
--
-- the JSON package indexer

hayooFJIndexer :: HIO FJ.FctCrawlerState
hayooFJIndexer
    = do o <- ask
         liftIOC $ stdIndexer
                    (config o)
                    (ao_resume o)
                    hayooStart
                    FJ.emptyFctState
    where
      config0 o
          = FJ.indexCrawlerConfig
            flush
            (ao_crawlPar o)
            (hayooRefs True $ ao_packages o)
            Nothing
            (Just $ checkDocumentStatus >>> prepareHaddock)
            (Just $ hayooGetTitle)
            (Just $ hayooGetFctInfo)
            hayooIndexContextConfig
          where
            flush
                = case ao_JSONserv o of
                    Nothing -> FJ.flushToFile
                    Just u  -> FJ.flushToServer u

      config o
          = ao_crawlFcJ o $
            setCrawlerTraceLevel ct ht   $
            setCrawlerSaveConf si sp     $
            setCrawlerMaxDocs md mp mt   $
            setCrawlerPreRefsFilter noHaddockPage $
            config0                      $ o
          where
            (ct, ht)      = ao_crawlLog o
            si            = ao_crawlSav o
            sp            = ao_crawlSfn o
            (md, mp, mt)  = ao_crawlDoc o

-- ------------------------------------------------------------

hayooIndexer :: HIO HayooIndexerCrawlerState
hayooIndexer
    = do o <- ask
         liftIOC $ stdIndexer
                    (config o)
                    (ao_resume o)
                    hayooStart
                    emptyHolumbusState
    where
    config0 o
        = indexCrawlerConfig
          (ao_crawlPar o)
          (hayooRefs True $ ao_packages o)
          Nothing
          (Just $ checkDocumentStatus >>> prepareHaddock)
          (Just $ hayooGetTitle)
          (Just $ hayooGetFctInfo)
          hayooIndexContextConfig

    config o
        = ao_crawlFct o $
          setCrawlerTraceLevel ct ht $
          setCrawlerSaveConf si sp   $
          setCrawlerSaveAction partA $
          setCrawlerMaxDocs md mp mt $
                                  -- haddock pages don't need to be scanned for new URIs
          setCrawlerPreRefsFilter noHaddockPage $
          config0                    $ o
        where
          xout              = ao_xml      o
          (ct, ht)          = ao_crawlLog o
          si                = ao_crawlSav o
          sp                = ao_crawlSfn o
          (md, mp, mt)      = ao_crawlDoc o
          partA
              | ao_partix o = writePartialIndex (not . null $ xout)
              | otherwise   = const $ return ()

noHaddockPage :: IOSArrow XmlTree XmlTree
noHaddockPage
    = fromLA $
      hasAttrValue transferURI (not . isHaddockURI) `guards` this

-- ------------------------------------------------------------

notice :: MonadIO m => [String] -> m ()
notice = noticeC "hayoo"

-- ------------------------------------------------------------

evalOptions :: String -> [String] -> AppOpts
evalOptions pn args
    = foldl (.) (ef1 . ef2) opts $ initAppOpts { ao_progname = pn }
    where
    (opts, ns, es)   = getOpt Permute hayooOptDescr args
    ef1
        | null es    = id
        | otherwise  = \ x -> x { ao_help   = True
                                , ao_msg = concat es
                                }
        | otherwise  = id
    ef2
        | null ns    = id
        | otherwise  = \ x -> x { ao_help   = True
                                , ao_msg = "wrong program arguments: " ++ unwords ns
                                }

-- ------------------------------------------------------------

hayooOptDescr :: [OptDescr (AppOpts -> AppOpts)]
hayooOptDescr
    = [ Option "h?" ["help"]
        ( NoArg $
          \ x -> x { ao_help     = True }
        )
        "usage info"

      , Option "" ["fct-index"]
        ( NoArg $
          \ x -> x { ao_pkgIndex = False
                   , ao_crawlSfn = "./tmp/ix-"
                   }
        )
        "process index for haddock functions and types (default)"

      , Option "" ["pkg-index"]
        ( NoArg $
          \ x -> x { ao_pkgIndex = True
                   , ao_crawlSfn = "./tmp/pkg-"
                   }
        )
        "process index for hackage package description pages"

      , Option "" ["cache"]
        ( NoArg $
          \ x -> x { ao_action   = BuildCache }
        )
        "update the cache"

      , Option "i" ["index"]
        ( ReqArg
          (\ f x -> x { ao_index = f })
          "INDEX"
        )
        "index input file (binary format) to be operated on"

      , Option "n" ["new-index"]
        ( ReqArg
          (\ f x -> x { ao_ixout = f })
          "NEW-INDEX"
        )
        "new index file (binary format) to be generatet, default is index file"

      , Option "s" ["new-search"]
        ( ReqArg
          (\ f x -> x { ao_ixsearch = f })
          "SEARCH-INDEX"
        )
        "new search index files (binary format) ready to be used by Hayoo! search"

      , Option "x" ["xml-output"]
        ( ReqArg
          (\ f x -> x { ao_xml = f })
          "XML-FILE"
        )
        "output of final crawler state in xml format, \"-\" for stdout"

      , Option "j" ["json-output"]
        ( NoArg
          (\ x -> x { ao_JSON = True })
        )
        "output of crawler results in JSON format, default: output to files"

      , Option "" ["json-server"]
        ( ReqArg
          (\ u x -> x { ao_JSONserv = Just $ if null u then defaultServer else u})
          "URI"
        )
        ( "the server, into which the JSON output will be pushed, default is " ++ show defaultServer ++ " (no file output)")

      , Option "r" ["resume"]
        ( ReqArg (\ s x -> x { ao_resume = Just s})
          "FILE"
        )
        "resume program with file containing saved intermediate state"

      , Option "p" ["packages"]
        ( ReqArg
          (\ l x -> x { ao_packages = pkgList l })
          "PACKAGE-LIST"
        )
        "packages to be processed, a comma separated list of package names"

      , Option "u" ["update"]
        ( NoArg $
          \ x -> x { ao_action   = UpdatePkg }
        )
        "update packages specified by \"packages\" option"

      , Option "d" ["delete"]
        ( NoArg $
          \ x -> x { ao_action   = RemovePkg }
        )
        "delete packages specified by \"packages\" option"

      , Option "" ["maxdocs"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlDoc = setMaxDocs i $
                                                                 ao_crawlDoc x }))
          "NUMBER"
        )
        "maximum # of docs to be processed"

      , Option "" ["maxthreads"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlDoc = setMaxThreads i $
                                                                 ao_crawlDoc x }))
          "NUMBER"
        )
        ( "maximum # of parallel threads, 0: sequential, 1: single thread with binary merge," ++
          " else real parallel threads, default: 1" )

      , Option "" ["maxpar"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlDoc = setMaxParDocs i $
                                                                 ao_crawlDoc x }))
          "NUMBER"
        )
        "maximum # of docs indexed at once before the results are inserted into index, default: 1024"

      , Option "" ["valid"]
        ( ReqArg (setOption parseTime (\ x t -> x { ao_crawlPar = setDocAge t $
                                                                  ao_crawlPar x }))
          "DURATION"
        )
        ( "validate cache for pages older than given time, format: " ++
          "10sec, 5min, 20hours, 3days, 5weeks, 1month, default is 1month" )

      , Option "" ["latest"]
        ( ReqArg (setOption parseTime (\ x t -> x { ao_latest   = Just t }))
          "DURATION"
        )
        "select latest packages newer than given time, format like in option \"valid\""

      , Option "" ["partition"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_partix    = True
                                                 , ao_crawlSav  = i }))
          "NUMBER"
        )
        "partition the index into smaller chunks of given # of docs and write the index part by part"

      , Option "" ["merge"]
        ( ReqArg (\ s x -> x { ao_action = MergeIx
                             , ao_resume = Just s })
          "FILE"
        )
        "merge chunks into final index, resume with latest crawler state"

      , Option "" ["save"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlSav  = i }))
          "NUMBER"
        )
        "save intermediate results of index, default is 5000"

      , Option "" ["defragment"]
        ( NoArg $
          \ x -> x { ao_defrag    = True }
        )
        "defragment index after delete or update"

      , Option "" ["hackage"]
        ( NoArg $
          \ x -> x { ao_getHack   = True }
        )
        "when processing latest packages, first update the package list from hackage"

      , Option "" ["ranking"]
        ( NoArg $
          \   x -> x { ao_pkgRank   = True }
        )
        "when processing package index, compute package rank, default is no rank"

      , Option "" ["only-ranking"]
        ( NoArg $
          \   x -> x { ao_pkgRank     = True
                     , ao_pkgRankOnly = True
                     }
        )
        "when processing package index for JSON, only compute package rank, no descriptions"
      ]
    where
    pkgList
        = words . map (\ x -> if x == ',' then ' ' else x)

    setOption parse f s x
        = either (\ e -> x { ao_msg  = e
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
