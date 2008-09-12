module Main(main)
where
import           Control.Monad
import           Text.XML.HXT.Arrow
-- import           Holumbus.Build.Config
-- import           Holumbus.Build.Crawl
-- import           Holumbus.Build.Index
-- import           Holumbus.Index.Common
-- import           Holumbus.Index.Documents 
-- import           Holumbus.Index.Inverted.Memory(emptyInverted)
-- import           Holumbus.Index.Cache
-- import           Holumbus.Utility

import           Holumbus.Common.Logging
import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.Distribution.DMapReduce

-- import qualified Data.IntMap as IM
-- import qualified Data.Set    as S
-- import           Data.Maybe
-- import           Data.Binary
-- import           Data.List

import Examples.Crawler.Crawl
import Examples.Crawler.Config

main :: IO ()
main 
  = do
    initializeLogging
    p <- newPortRegistryFromXmlFile "registry.xml"
    setPortRegistry p
  
    let cc = getConfig
    -- ---------------------------------------------------------------------------------------------
    -- CRAWLING
    -- ---------------------------------------------------------------------------------------------
    
    mr <- mkMapReduceClient defaultMRClientConfig 
    
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl (cc_TraceLevel cc) (cc_DocsPerCrawl cc) (cc_CrawlerState cc) mr
    -- localDocs <- return $ tmpDocs (fromMaybe "/tmp" (ic_tempPath idxConfig)) docs

    -- writeToBinFile ( (ic_indexPath idxConfig) ++ "-docs.bin") (docs)
 
    runX (traceMsg 0 (" docs ----------------------------- " ))
    runX (traceMsg 0 (show docs))

    return ()

{-
    

    -- ---------------------------------------------------------------------------------------------
    -- INDEXING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" indexing  ----------------------------- " ))

    c <- createCache ((ic_indexPath idxConfig) ++ "-cache.db")

    idx <- buildIndex workerThreads traceLevel localDocs idxConfig emptyInverted (Just c)

    writeToXmlFile ( (ic_indexPath idxConfig) ++ "-index.xml") idx
    writeToBinFile ( (ic_indexPath idxConfig) ++ "-index.bin") idx

    return()
    where
      mergeIndexes' i1 f = do
                           i2 <- loadFromBinFile (f  ++ "-index.bin")
                           return $ mergeIndexes i1 i2
-}

{-                         
fromDocuments :: Binary a => CrawlerState Documents a -> Documents a -> CrawlerState Documents a
fromDocuments cs ds = cs { cs_toBeProcessed = S.fromList ( map (uri . snd) ( IM.toList $ toMap ds )) }     
-}      
        
