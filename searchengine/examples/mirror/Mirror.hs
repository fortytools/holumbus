{-# OPTIONS_GHC -XFlexibleInstances #-}

module Main
where

-- import           Text.XML.HXT.XPath
import           Text.XML.HXT.Arrow
import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
-- import           Holumbus.Build.Index
import           Holumbus.Index.Common
import           Holumbus.Index.Documents

import           Control.Monad
import           Data.Binary

import qualified Data.IntMap as IM
import qualified Data.Set as S
-- import qualified Data.Map as M

import           Data.Maybe
-- import           Data.DateTime
-- import           Data.Time.Clock
import           System.Directory

data PageInfo = PageInfo
--  { lastCrawled :: DateTime
  { try         :: Integer
  }
  deriving (Show)

{-
instance Binary UTCTime where
  put (UTCTime d dt) = put d >> put dt
  get = liftM2 UTCTime get get
-}

instance Binary PageInfo where
  put (PageInfo l ) = put l -- >> put t
  get = liftM PageInfo get
  --  get = liftM2 PageInfo get get

instance XmlPickler PageInfo where
  xpickle = xpZero -- xpAttr "try" xpText0 
  
  {- xpWrap (fromTuple, toTuple) xpPage
    where
      fromTuple (l, t) = PageInfo l t
      toTuple (PageInfo l t) = (l, t)
      xpPage = xpPair xpLastCrawled xpTry
      xpLastCrawled = xpAttr "lastCrawled" xpText0
      xpTry = xpAttr "try" xpText0
  -}
main :: IO ()
main 
  = do
    dir <- getHomeDirectory
    let traceLevel    = 1
        workerThreads = 1
        docsPerCrawl  = 32
        indexPath     = dir ++ "/indexes/mirror"
        idxConfig     = ic_HTTP {ic_indexPath = indexPath}
    
    createDirectoryIfMissing True (dir ++ "/indexes/")
    e <- doesFileExist (indexPath ++ "-docs.bin")

    docs <- 
      if e
        then do
	     docs' <- loadFromBinFile (indexPath ++ "-docs.bin")
	     let cs' = initialCrawlerState idxConfig docs' customFunction
	         cs  = cs' { cs_toBeProcessed = S.union (cs_toBeProcessed cs')
                                                        (docsToSet docs')
			   }
             crawled <- crawl traceLevel workerThreads docsPerCrawl cs
	     
	     return $ insertMissingDocs crawled
	     		(S.difference (docsToSet docs') (docsToSet crawled))
	     
	     
	     return $ crawled
	     
        else crawl traceLevel workerThreads docsPerCrawl
	     	(initialCrawlerState idxConfig emptyDocuments customFunction)
	

    writeToBinFile  ( (ic_indexPath idxConfig) ++ "-docs.bin") (docs)
    writeToXmlFile ( (ic_indexPath idxConfig) ++  "-docs.xml") (docs)

    return()

insertMissingDocs :: (HolDocuments d a, Binary a) => d a -> S.Set URI -> d a
insertMissingDocs docs uris
  = S.fold update docs uris
    where
      update :: (HolDocuments d a, Binary a) => URI -> d a -> d a
      update u ds 
        = let id  = (fromJust $ lookupByURI ds u)
	      doc = (lookupById ds id )
	  in updateDoc docs id (doc {custom = (custom doc) + 1 })
    
docsToSet :: (HolDocuments d a, Binary a) => d a -> S.Set URI
docsToSet docs = S.fromList $ map (uri . snd) (IM.toList $ toMap docs)

    
customFunction :: ArrowXml a => a XmlTree (Maybe Int)
customFunction = constA $ Just $ 1
      

ic_HTTP :: IndexerConfig
ic_HTTP 
  = IndexerConfig
    { ic_startPages     = [ "http://hackage.haskell.org/packages/archive/HTTP/latest/doc/html/Network-Browser.html"]
    , ic_tempPath        = Just "/tmp/cache/http/"
    , ic_indexPath        = "/home/sms/indexes/http"
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs = [] -- ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://hackage.haskell.org/packages/archive/HTTP/latest/doc/html/"]
                                            [ "/src/"]
    }  


ic_haskellwiki :: IndexerConfig
ic_haskellwiki
  = IndexerConfig 
    { ic_startPages     = [ "http://haskell.org"
                          , "http://haskell.org/haskellwiki/Special:Allpages/5_ad%C4%B1mda_Haskell"
                          , "http://haskell.org/haskellwiki/?title=Special:Allpages&from=Introduction_to_IO"
                          ]
    , ic_indexerTimeOut = 120 * 1000000 -- two minutes
    , ic_tempPath       = Just "/tmp/cache/wiki/"
    , ic_indexPath      = "/tmp/" -- will be replaced
    , ic_contextConfigs = []
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://(www)?\\.haskell\\.org" ]             -- allow
                                            [ "/pipermail/"                                 -- deny
                                            , "/docs/"  -- don't mess with hayoo
                                            , "&oldid=" -- no old revisions
                                            , "action=history"
                                            , "action=edit"
					    , "pkg$"
					    , "bz2$"
					    , "pkg\\.gz$"
                                            ]
    }
        
    