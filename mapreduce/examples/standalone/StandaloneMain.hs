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

import Control.Exception

import qualified Holumbus.Common.Logging as LOG
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.MapReduce.Demo as DEMO
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.Standalone.Standalone as SA
import qualified Holumbus.Standalone.UserInterface as UI


version :: String
version = "Holumbus-Standalone MapReducer 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      LOG.initializeLogging      
      sa <- initializeData
      UI.runUI sa version
      deinitializeData sa


initializeData :: IO SA.Standalone
initializeData
  = do
    let maps = DEMO.demoMapFunctions
    let reduces = DEMO.demoReduceFunctions
    let partitions = DEMO.demoPartitionFunctions
    -- fs <- FS.standaloneFileSystem "storage/" Nothing
    -- DEMO.createDemoFiles fs
    SA.newStandalone maps reduces partitions


deinitializeData :: SA.Standalone -> IO ()
deinitializeData sa
  = do
    SA.closeStandalone sa