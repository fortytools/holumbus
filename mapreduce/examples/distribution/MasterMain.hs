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

import           Holumbus.Common.Logging
import           Holumbus.Network.Site
import qualified Holumbus.Network.Port as Port
import qualified Holumbus.MapReduce.Demo as DEMO
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.Distribution.Master.MasterData as Master
import qualified Holumbus.Distribution.Master as MC
import qualified Holumbus.Distribution.Distribution as D
import qualified Holumbus.MapReduce.UserInterface as UI


version :: String
version = "Holumbus-Distribution Standalone-Master 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging      
      d <- initializeData
      UI.runUI d version      
      deinitializeData d


initializeData :: IO (D.Distribution)
initializeData 
  = do
    sid <- getSiteId
    putStrLn $ "initialising master on site" ++ show sid 
    putStrLn "-> master-port"
    let maps = DEMO.demoMapActions
    let reduces = DEMO.demoReduceActions
    fs <- FS.mkSingleController "controller.port"
    master <- Master.newMaster fs maps reduces
    let port = MC.getMasterRequestPort master
    Port.writePortToFile port "master.port"
    putStrLn "-> distribution"
    d <- D.newDistribution master
    return d


deinitializeData :: D.Distribution -> IO ()
deinitializeData d
  = do
    D.closeDistribution d
    
