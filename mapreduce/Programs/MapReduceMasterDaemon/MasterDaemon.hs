-- ----------------------------------------------------------------------------
{- |
  Module     : MasterDaemon
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Daemonized Version of MASter with a tcp console
-}
-- ----------------------------------------------------------------------------

module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils                         ( handleAll )

import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.FileSystem.FileSystem                 as FS
import qualified Holumbus.Distribution.DMapReduce               as MR
import           Holumbus.Distribution.Master.MasterState
import qualified Holumbus.MapReduce.DaemonInterface             as UI
import           System.Environment
import           System.Log.Logger
import           System.Exit


version :: String
version = "Daemon-Master 0.1"

prompt :: String
prompt = "# "++version++" > "

localLogger :: String
localLogger = "MasterDaemon"

pUsage :: IO ()
pUsage = do
  putStrLn "Usage: MasterDaemon ConsolePort Logfile Statefile"

params :: IO [String]
params = do
  args <- getArgs
  if length args /= 3 then do
    errorM localLogger "Wrong argument count"
    pUsage
    exitFailure
    else
      return args
      
main :: IO ()
main
  = do
    putStrLn version
    handleAll (\e -> errorM localLogger $ "EXCEPTION: " ++ show e) $
      do      
      (s_cport:logfile:statefile:[]) <- params
      initializeFileLogging logfile [(localLogger, INFO),("Holumbus.Network.DoWithServer",INFO)]
      addResource statefile
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"      
      setPortRegistry p
      (mr,fs) <- initializeData
      UI.runDaemon mr version (read s_cport) prompt 
      deinitializeData (mr,fs)


initializeData :: IO (MR.DMapReduce, FS.FileSystem)
initializeData 
  = do    
    fs <- FS.mkFileSystemController FS.defaultFSControllerConfig
    let config  = MR.defaultMRMasterConfig
    mr <- MR.mkMapReduceMaster fs config
    return (mr,fs)


deinitializeData :: (MR.DMapReduce, FS.FileSystem) -> IO ()
deinitializeData (mr,fs)
  = do
    MR.closeMapReduce mr
    FS.closeFileSystem fs

-- ----------------------------------------------------------------------------
