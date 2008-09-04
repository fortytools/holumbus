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

import           Holumbus.Common.Logging
import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.Distribution.DMapReduce as MR
import qualified Holumbus.MapReduce.UserInterface as UI
import qualified Holumbus.MapReduce.Demo as DEMO



version :: String
version = "Holumbus-Distribution Standalone-Worker 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging
      p <- newPortRegistryFromXmlFile "registry.xml"
      setPortRegistry p
      d <- initializeData
      UI.runUI d version      
      deinitializeData d


initializeData :: IO (MR.DMapReduce)
initializeData 
  = do
    fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig

    let actions = DEMO.demoActions
    let config  = MR.defaultMRWorkerConfig
    MR.mkMapReduceWorker fs actions config


deinitializeData :: MR.DMapReduce -> IO ()
deinitializeData mr
  = do
    MR.closeMapReduce mr
    
