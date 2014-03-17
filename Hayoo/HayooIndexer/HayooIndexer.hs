{-# LANGUAGE FlexibleContexts #-}

-- ------------------------------------------------------------

module Main (main)
where

import           Codec.Compression.BZip     (compress, decompress)

import           Control.Applicative        ((<$>))
import           Control.DeepSeq
import           Control.Monad.Error
import           Control.Monad.IO.Class     ()
import           Control.Monad.Reader

import qualified Data.Binary                as B
import           Data.Char
import           Data.Function.Selector
-- import           Data.List                   (intercalate)
import           Data.Maybe
import           Data.Size
import           Data.Time                  (getCurrentTime)
import           Data.Typeable

import           Hayoo.HackagePackage
import           Hayoo.Haddock
import qualified Hayoo.Hunt.FctIndexerCore  as FJ
import           Hayoo.Hunt.IndexSchema
import           Hayoo.Hunt.Output          (defaultServer, outputValue)
import qualified Hayoo.Hunt.PkgIndexerCore  as PJ
import           Hayoo.IndexConfig
import           Hayoo.IndexerCore
import           Hayoo.IndexTypes
import           Hayoo.PackageArchive
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
    = Usage
    | BuildIx | UpdatePkg | RemovePkg | BuildCache | MergeIx
    | CreateSchema | DeleteSchema
      deriving (Eq, Show)

data AppOpts
    = AO
      { ao_progname    :: String
      , ao_index       :: String
      , ao_ixout       :: String
      , ao_ixsearch    :: String
      , ao_xml         :: String
      , ao_pkgIndex    :: Bool
      , ao_JSON        :: Bool
      , ao_JSONserv    :: Maybe String
      , ao_JSONmax     :: Int
      , ao_JSONmaxsave :: Maybe Int
      , ao_action      :: AppAction
      , ao_defrag      :: Bool
      , ao_partix      :: Bool
      , ao_resume      :: Maybe String
      , ao_packages    :: Maybe [String]
      , ao_pkgregex    :: Maybe String
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
      , ao_pkgIndex     = False
      , ao_JSON         = False
      , ao_JSONserv     = Nothing
      , ao_JSONmax      = 1
      , ao_JSONmaxsave  = Nothing
      , ao_action       = Usage
      , ao_defrag       = False
      , ao_partix       = False
      , ao_resume       = Nothing
      , ao_packages     = Nothing
      , ao_pkgregex     = Nothing
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
    = do (a, pn) <- asks (ao_action &&& ao_progname)
         case a of
           Usage
               -> do msg <- asks ao_msg
                     liftIO $ hPutStrLn stderr (msg ++ "\n" ++ usageInfo pn hayooOptDescr)
                     if null msg
                       then return ()
                       else throwError "wrong option"
           _   -> do asks (snd . ao_crawlLog) >>= setLogLevel ""
                     asks ao_getHack          >>= getHackageIndex
                     local (\ opts -> opts {ao_getHack = False}) $
                       case a of
                         BuildCache   -> mainCache
                         MergeIx      -> mainHaddock
                         CreateSchema -> indexSchema execCreateHayooIndexSchema
                         DeleteSchema -> indexSchema execDropHayooIndexSchema
                         _            -> do p <- asks ao_pkgIndex
                                            if p
                                              then mainHackage
                                              else mainHaddock

-- ------------------------------------------------------------

getHackageIndex :: Bool -> HIO ()
getHackageIndex False
    = return ()
getHackageIndex True
    = do notice ["update 00-index.tag.gz from hackage"]
         liftIO $ updateArchiveFile

indexSchema :: (Maybe String -> HIO ()) -> HIO ()
indexSchema out
    = asks ao_JSONserv >>= out

-- ------------------------------------------------------------

mainCache :: HIO ()
mainCache
    = do action
    where
      action
          = withPackages False action2

      action2
          = do pl <- asks ao_packages
               case pl of
                 Nothing -> action3
                 Just xs -> updatePkg xs

      action3
          = do rs <- asks ao_resume
               notice $ if isJust rs
                        then ["resume cache update"]
                        else ["cache hayoo pages"]
               hayooCacher >>= writeResults

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
    = do js <- asks ao_JSON
         rk <- asks ao_pkgRank
         if js && not rk
            then mainHackageJSON
            else mainHackage'

mainHackageJSON :: HIO ()
mainHackageJSON
    = do action
    where
      action
          = withPackages True action2

      action2
          = do act <- asks ao_action
               ps <- (maybe [] id) <$> asks ao_packages
               case act of
                 BuildIx
                     -> indexPkg ps
                 _   -> notice ["mainHackageJSON:", "ignoring command"]

      indexPkg :: [String] -> HIO ()
      indexPkg ps
          = do notice $ if null ps
                          then ["JSON indexing all packages from hackage package index"]
                          else "JSON indexing hackage package descriptions for packages:" : ps
               ix <- getS theResultAccu <$> hayooPJIndexer
               notice ["writing package index as JSON"]
               flushJSON "00-packages" ix
               notice ["package index as JSON written"]

      flushJSON :: String -> PJ.PkgIndexerState -> HIO ()
      flushJSON pkg ix
          = do serv <- asks ao_JSONserv
               save <- maybe False (const True) <$> asks ao_JSONmaxsave
               ct <- liftIO $ getCurrentTime
               notice $ "flushing package index as JSON to" : target serv
               outputValue (dest serv) (PJ.toCommand save ct True ix)
               notice $ ["flushing package index as JSON done"]
               return ()
          where
            target (Just uri) = ["server", show uri]
            target  Nothing   = ["file",   show pkg]
            dest  (Just uri)  = Right uri
            dest   Nothing    = Left pkg

mainHackage' :: HIO ()
mainHackage'
    = withPackages False action
    where
      action
          = do ac <- asks ao_action
               ps <- (maybe [] id) <$> asks ao_packages
               case ac of
                 RemovePkg
                     -> if null ps
                        then noaction
                        else removePkg ps >>= writeRes
                 UpdatePkg
                     -> if null ps
                        then noaction
                        else updatePkg ps >>= writeRes
                 _
                     -> do rs <- asks ao_resume
                           if isJust rs
                              then notice ["resume hackage package description indexing"]
                              else return ()
                           js <- asks ao_JSON
                           if js        -- JSON package rank is computed here
                              then indexPkgJSON ps
                              else (indexPkg ps >>= writeRes)

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

      indexPkgJSON :: [String] -> HIO () -- HayooPkgIndexerState
      indexPkgJSON []
          = do notice ["compute JSON format package ranking"]
               (getS theResultAccu `fmap` hayooPkgIndexer) >>= (rankPkgJSON . ixs_documents)

      indexPkgJSON ps
          = do notice ["package rank computing only possible for all packages,"
                      ,"not for a selection", show ps
                      ]

      rankPkgJSON :: Documents PackageInfo -> HIO ()
      rankPkgJSON dt
          = do rank <- asks ao_pkgRank
               serv <- asks ao_JSONserv
               save <- maybe False (const True) <$>
                       asks ao_JSONmaxsave
               now  <- liftIO getCurrentTime
               if rank
                  then do notice ["computing package ranks"]
                          outputValue (dest serv) (PJ.rankToCommand save now $ packageDocRanking dt)
                          notice ["JSON package ranks written"]
                  else do notice ["no package ranks computed"]
          where
            dest Nothing    = Left "00-ranking"
            dest (Just uri) = Right uri

-- ------------------------------------------------------------

mainHaddock :: HIO ()
mainHaddock
    = do js <- asks ao_JSON
         if js
            then mainHaddockJSON
            else mainHaddock'

mainHaddockJSON :: HIO ()
mainHaddockJSON
    = withPackages True action
    where
      action
          = do act <- asks ao_action
               ps  <- (maybe [] id) <$>
                      asks ao_packages
               case act of
                 BuildIx
                     -> indexPkgList ps
                 _   -> notice ["mainHaddockJSON:", "ignoring command"]

      indexPkgList :: [String] -> HIO ()
      indexPkgList []  = noaction
      indexPkgList ps' = do mx <- asks ao_JSONmax
                            ms <- maybe maxBound id <$> asks ao_JSONmaxsave
                            sequence_ . (map $ uncurry indexPkg) $ part mx ms ps'
          where
            part :: Int -> Int -> [String] -> [(Bool, [String])]
            part n s = part' 0
                where
                  part' _ [] = []
                  part' i xs = (b, x) : part' i' xs'
                      where
                        (x, xs') = splitAt n xs
                        i' = if b then 0 else i + n
                        b  = (i + n >= s)                -- maximum reached
                             ||
                             (s /= maxBound && null xs') -- last chunk

      indexPkg :: Bool -> [String] -> HIO ()
      indexPkg save pkgs
          = do notice ["start indexing haddock pages in JSON for package:", show pkgs]
               local (\ o -> o { ao_packages = Just pkgs }
                     ) $ do ix <- getS theResultAccu <$> hayooFJIndexer
                            flushJSON save pkgs ix

               notice ["finish indexing haddock pages in JSON for package:", show pkgs]
               return ()

      flushJSON :: Bool -> [String] -> FJ.FctIndexerState -> HIO ()
      flushJSON save pkgs ix
          = do serv <- asks ao_JSONserv
               ct <- liftIO $ getCurrentTime
               notice $ "flushing function index as JSON to" : target serv
               outputValue (dest serv) (FJ.toCommand save ct True pkgs ix)
               notice $ ["flushing function index as JSON done"]
               return ()
          where
            target (Just uri) = ["server", show uri]
            target  Nothing   = ["file",   show $ fn pkgs]
            dest  (Just uri)  = Right uri
            dest   Nothing    = Left $ fn pkgs
            fn [pkg]          = pkg
            fn xs             = head xs ++ ".." ++ last xs

mainHaddock' :: HIO ()
mainHaddock'
    = withPackages False action
    where
      action
          = do latest <- asks ao_latest
               maybe action2 updateLatest' latest
      action2
          = do apl <- asks ao_action
               ps  <- asks ao_packages
               case apl of
                 RemovePkg
                     -> maybe noaction
                              (\x -> removePkg x >>= writeRes)
                              ps
                 UpdatePkg
                     -> maybe noaction
                              (\x -> updatePkg x >>= writeRes)
                              ps
                 MergeIx
                     -> loadPartialIx >>= mergeAndWritePartialRes
                 _
                     -> do rs <- asks ao_resume
                           if isJust rs
                              then notice ["resume haddock document indexing"]
                              else return ()
                           ((indexPkg $ maybe [] id ps) >>= writePartialRes)

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
                            , ao_packages = Nothing
                            }) (snd `fmap` indexPkg [])

      indexPkg :: [String] -> HIO (HayooIndexerState, [Int])
      indexPkg ps
          = do notice $ if null ps
                          then ["indexing all haddock pages"]
                          else "indexing haddock for packages:" : ps
               (getS (theResultAccu .&&&. theListOfDocsSaved) `fmap` hayooIndexer)

      updateLatest' latest
          = do notice ["reindex with latest packages"]
               ps  <- liftIO $ getNewPackages latest
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

getPackages :: Bool -> HIO (Maybe [String])
getPackages allPkgs
    = do pl <- asks ao_packages
         r  <- asks ao_pkgregex
         ls <- asks ao_latest
         pl1 <- packageList pl ls
         pl2 <- filterRegex r pl1
         case pl2 of
           Just xs -> notice ["packages to be processed:", show xs]
           Nothing -> notice ["all packages to be processed"]
         return pl2
    where
      packageList :: Maybe [String] -> Maybe Int -> HIO (Maybe [String])
      packageList _ (Just age)                                  -- eval option --latest
          = do notice ["compute list of latest packages"]
               res <- liftIO $ getNewPackages age
               notice ["latest packages:", show res]
               return $ Just res

      packageList Nothing _                                   -- compute default package list
          | allPkgs
              = Just <$> (liftIO $ getNewPackages 0)
          | otherwise
              = return Nothing

      packageList ps _                                        -- explicit list --packages
          = return ps

      filterRegex :: Maybe String -> Maybe [String] -> HIO (Maybe [String])
      filterRegex _ Nothing
          = return Nothing

      filterRegex Nothing ps
          = return ps

      filterRegex (Just rex) (Just ps)
          = do notice ["filtered package list:", show ps]
               return $ Just res
            where
              res = filter (match rex) ps

withPackages :: Bool -> HIO a -> HIO a
withPackages allPkgs act
    = do ps <- getPackages allPkgs
         local (\opts -> opts
                         { ao_latest   = Nothing
                         , ao_pkgregex = Nothing
                         , ao_packages = ps
                         }
               ) act

-- ------------------------------------------------------------

removePacks :: (B.Binary di, NFData di) =>
                   (Document di -> String) -> HIO (HolumbusState di)
removePacks getPkgName'
    = do (ix, dfg) <- asks (ao_index &&& ao_defrag)
         pkg <- (maybe [] id) <$> asks ao_packages
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

writePartialRes :: (HayooIndexerState, [Int]) -> HIO ()
writePartialRes (x, ps)
    = do part <- asks ao_partix
         if part
            then mergeAndWritePartialRes ps
            else writeRes x

mergeAndWritePartialRes :: [Int] -> HIO ()
mergeAndWritePartialRes ps
    = do pxs <- (\ fn -> map (mkTmpFile 10 fn) ps) <$> asks ao_crawlSfn
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
          (hayooRefs False $ (maybe [] id $ ao_packages o))
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
            (ao_crawlPar o)
            (hayooRefs False $ (maybe [] id $ ao_packages o))
            Nothing
            (Just $ checkDocumentStatus >>> preparePkg)
            (Just $ hayooGetPkgTitle)
            (Just $ hayooGetPkgInfo)
            hayooPkgIndexContextConfig

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
            (ao_crawlPar o)
            (hayooRefs True $ (maybe [] id $ ao_packages o))
            Nothing
            (Just $ checkDocumentStatus >>> prepareHaddock)
            (Just $ hayooGetTitle)
            (Just $ hayooGetFctInfo)
            hayooIndexContextConfig

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
          (hayooRefs True $ (maybe [] id $ ao_packages o))
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
        | otherwise  = \ x -> x { ao_action = Usage
                                , ao_msg    = concat es
                                }
        | otherwise  = id
    ef2
        | null ns    = id
        | otherwise  = \ x -> x { ao_action = Usage
                                , ao_msg = "wrong program arguments: " ++ unwords ns
                                }

-- ------------------------------------------------------------

hayooOptDescr :: [OptDescr (AppOpts -> AppOpts)]
hayooOptDescr
    = [ Option "h?" ["help"]
        ( NoArg $
          \ x -> x { ao_action = Usage }
        )
        "usage info"

      , Option "" ["fct-index"]
        ( NoArg $
          \ x -> x { ao_action = BuildIx
                   , ao_pkgIndex = False
                   , ao_crawlSfn = "./tmp/ix-"
                   }
        )
        "process index for haddock functions and types (default)"

      , Option "" ["pkg-index"]
        ( NoArg $
          \ x -> x { ao_action = BuildIx
                   , ao_pkgIndex = True
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

      , Option "r" ["resume"]
        ( ReqArg (\ s x -> x { ao_resume = Just s})
          "FILE"
        )
        "resume program with file containing saved intermediate state"

      , Option "p" ["packages"]
        ( ReqArg
          (\ l x -> x { ao_packages = Just $ pkgList l }
          )
          "PACKAGE-LIST"
        )
        "packages to be processed, a comma separated list of package names"

      , Option "" ["pkg-regex"]
        ( ReqArg
          (\ l x -> x { ao_pkgregex = Just l }
          )
          "PACKAGE-REGEX"
        )
        "filter for packages to be processed, a regular expression pattern)"

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

      , Option "" ["json-create-schema"]
        ( NoArg $
          \ x -> x { ao_action   = CreateSchema
                   , ao_JSON     = True
                   }
        )
        "JSON command to create Hayoo index schema in Hunt server (--json-output implied)"

      , Option "" ["json-delete-schema"]
        ( NoArg $
          \ x -> x { ao_action   = DeleteSchema
                   , ao_JSON     = True
                   }
        )
        "JSON command to drop Hayoo index schema in Hunt server (--json-output implied)"

      , Option "" ["json-fct"]
        ( NoArg $
          \ x -> x { ao_action      = BuildIx
                   , ao_pkgIndex    = False
                   , ao_JSON        = True
                   , ao_pkgRank     = False
                   , ao_pkgRankOnly = False
                   }
        )
        "JSON command to index Haddock document pages in Hunt server (--json-output implied)"

      , Option "" ["json-pkg"]
        ( NoArg $
          \ x -> x { ao_action      = BuildIx
                   , ao_pkgIndex    = True
                   , ao_JSON        = True
                   , ao_pkgRank     = False
                   , ao_pkgRankOnly = False
                   }
        )
        "JSON command to index Hayoo package descriptions in Hunt server (--json-output implied)"

      , Option "" ["json-pkg-rank"]
        ( NoArg $
          \ x -> x { ao_action      = BuildIx
                   , ao_pkgIndex    = True
                   , ao_JSON        = True
                   , ao_pkgRank     = True
                   , ao_pkgRankOnly = True
                   }
        )
        "JSON command to compute Hayoo package rank in Hunt server (--json-output implied)"

      , Option "j" ["json-output"]
        ( NoArg
          (\ x -> x { ao_JSON = True })
        )
        "output of crawler results in JSON format, default: output to files in subdir \"./json/\""

      , Option "" ["json-server"]
        ( ReqArg
          (\ u x -> x { ao_JSONserv = Just $ if null u then defaultServer else u})
          "URI"
        )
        ( "the server, into which the JSON output will be pushed, default is " ++
          show defaultServer ++ " (no file output)"
        )

      , Option "" ["json-maxpkg"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_JSONmax = 1 `max` i }))
          "NUMBER"
        )
        "when indexing JSON, maximum # of packages indexed as a bundle, default is 1"

      , Option "" ["json-maxsave"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_JSONmaxsave = Just $ 1 `max` i }))
          "NUMBER"
        )
        "when indexing JSON, saving server state after indexing # of packages, default: infinity"

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
        ( ReqArg (setOption parseDuration (\ x t -> x { ao_crawlPar = setDocAge t $
                                                                  ao_crawlPar x }))
          "DURATION"
        )
        ( "validate cache for pages older than given time, format: " ++
          "10sec, 5min, 20hours, 3days, 5weeks, 1month, default is 1month" )

      , Option "" ["latest"]
        ( ReqArg (setOption parseDuration (\ x t -> x { ao_latest   = Just t }))
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
        = either (\ e -> x { ao_msg    = e
                           , ao_action = Usage
                           }
                 ) (f x) . parse $ s

-- ------------------------------------------------------------

parseInt                                :: String -> Either String Int
parseInt s
    | match "[0-9]+" s                  = Right $ read s
    | otherwise                         = Left  $ "number expected in option arg"

parseDuration                           :: String -> Either String Int
parseDuration s
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
