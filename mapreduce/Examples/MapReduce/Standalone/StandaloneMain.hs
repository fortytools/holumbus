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
import qualified Holumbus.Standalone.SMapReduce as MR
import qualified Holumbus.MapReduce.UserInterface as UI


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


initializeData :: IO MR.SMapReduce
initializeData
  = do
    fs <- FS.mkStandaloneFileSystem FS.defaultFSStandaloneConfig
    DEMO.createDemoFiles fs
  
    let actions = DEMO.demoActions
    let config  = MR.defaultStandaloneConfig
    MR.newSMapReduce fs actions config


deinitializeData :: MR.SMapReduce -> IO ()
deinitializeData sa
  = do
    MR.closeMapReduce sa
