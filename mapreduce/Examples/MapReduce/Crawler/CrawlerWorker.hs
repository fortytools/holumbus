-- ----------------------------------------------------------------------------
{- |
  Module     : 
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Main(main) where

import           Control.Exception

-- import           Text.XML.HXT.Core

import qualified Holumbus.Data.KeyMap as KMap
import           Holumbus.MapReduce.Types

import           Holumbus.Common.Logging
import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.Distribution.DMapReduce as MR
import qualified Holumbus.MapReduce.UserInterface as UI

import           Examples.MapReduce.Crawler.Config
import           Examples.MapReduce.Crawler.Crawl
import           Examples.MapReduce.Crawler.Index

version :: String
version = "Holumbus-Distribution Standalone-Worker 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging []
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p
      (mr,fs) <- initializeData
      UI.runUI mr version      
      deinitializeData (mr,fs)


initializeData :: IO (MR.DMapReduce, FS.FileSystem)
initializeData 
  = do
    fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig

    let cc = getConfig        
    let actions = KMap.insert (readActionConfiguration $ indexerOccurrencesAction cc) $
                  KMap.insert (readActionConfiguration $ indexerBuildIndexAction) $
                  KMap.insert (readActionConfiguration $ crawlerAction cc) $ 
                  KMap.empty
    
    let config  = MR.defaultMRWorkerConfig
    mr <- MR.mkMapReduceWorker fs actions config
    return (mr,fs)


deinitializeData :: (MR.DMapReduce, FS.FileSystem) -> IO ()
deinitializeData (mr,fs)
  = do
    MR.closeMapReduce mr
    FS.closeFileSystem fs
    
