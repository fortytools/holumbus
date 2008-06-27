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

import Holumbus.Common.Logging
import Holumbus.Network.Site
import qualified Holumbus.Network.Port as Port
import qualified Holumbus.FileSystem.Node.NodeData as N
import qualified Holumbus.FileSystem.Controller.ControllerPort as CP
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.FileSystem.Storage.FileStorage as FST
import qualified Holumbus.FileSystem.UserInterface as UI


version :: String
version = "Holumbus-FileSystem Standalone-Node 0.1"


main :: IO ()
main
  = do
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging      
      fs <- initializeData
      UI.runUI fs version      
      deinitializeData fs


initializeData :: IO (FS.FileSystem)
initializeData 
  = do
    sid <- getSiteId
    putStrLn $ "initialising controller on site" ++ show sid 
    putStrLn "-> controller-port"
    p <- Port.readPortFromFile "controller.port"
    let cp = (CP.newControllerPort p)
    putStrLn "-> storage"
    let storage = FST.newFileStorage "storage/" Nothing
    putStrLn "-> node" 
    n <- N.newNode cp storage    
    putStrLn "-> fileSystem"
    fs <- FS.newFileSystem cp
    fs' <- FS.setFileSystemNode n fs
    return fs'


deinitializeData :: FS.FileSystem -> IO ()
deinitializeData fs
  = do
    FS.closeFileSystem fs
