module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils                         	( handleAll )

import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.FileSystem.FileSystem 		as FS
import qualified Holumbus.Distribution.DMapReduce		as MR
import qualified Holumbus.MapReduce.DaemonInterface		as UI
import           Examples2.Mandel.DMandel
import            System.Log.Logger
import            System.Environment
version :: String
version = "Distributed Mandel-set calculation"

getParams	:: IO [String]
getParams
    = do
      args <- getArgs
      return (args ++ repeat "")
      
main	:: IO()
main
    = do
      args <-{-#SCC "getargscc" #-}getParams
      {-#SCC "getargscc" #-} main2 args
      
main2 :: [String] -> IO ()
main2 ( sport : logfile : xs )
  = do
    putStrLn version
    handleAll (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeFileLogging logfile [("Holumbus", ERROR),("Examples2",ERROR)] --  [("Holumbus.MapReduce.Types",DEBUG),("Holumbus.FileSystem.FileSystem",WARNING)]
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p
      (mr,fs) <- initializeData sport
      UI.runDaemon mr version (read sport) "#> "
      deinitializeData (mr,fs)


initializeData :: String -> IO (MR.DMapReduce, FS.FileSystem)
initializeData port
  = do
    let name = "mandelworkerstorage_"++port
    fs <- {-# SCC "initializeDatad" #-}FS.mkFileSystemNode $ FS.FSNodeConf "FSController" Nothing (name++"/") "directory"
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
