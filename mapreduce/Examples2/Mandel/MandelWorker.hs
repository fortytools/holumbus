module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils                         	( handleAll )

import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.FileSystem.FileSystem 		as FS
import qualified Holumbus.Distribution.DMapReduce		as MR
import qualified Holumbus.MapReduce.UserInterface		as UI
import           Examples2.Mandel.DMandel
import            System.Log.Logger
version :: String
version = "Distributed Mandel-set calculation"


main :: IO ()
main
  = do
    putStrLn version
    handleAll (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging [("Holumbus.MapReduce.Types",WARNING),("Holumbus.FileSystem.FileSystem",WARNING)]
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p
      (mr,fs) <- initializeData
      UI.runUI mr version
      deinitializeData (mr,fs)


initializeData :: IO (MR.DMapReduce, FS.FileSystem)
initializeData 
  = do
    fs <- {-# SCC "initializeDatad" #-}FS.mkFileSystemNode FS.defaultFSNodeConfig
    let actions = dmandelActionMap
    let config  = MR.defaultMRWorkerConfig
    mr <- {-# SCC "initializeDatadddd" #-}MR.mkMapReduceWorker fs actions config
    return (mr,fs)


deinitializeData :: (MR.DMapReduce, FS.FileSystem) -> IO ()
deinitializeData (mr,fs)
  = do
    MR.closeMapReduce mr
    FS.closeFileSystem fs
    
-- ----------------------------------------------------------------------------
