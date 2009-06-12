-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Common.Logging
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


  In this part of the Holumbus-Framework, we use hslogging. With this, we are
  able to activate and deactivate some nice debugging-Output.
  This is the global configuration for the logging-output. Of cource, if you
  want to use this output, you have to invoke initializeLogging.

-}
-- ----------------------------------------------------------------------------

module Holumbus.Common.Logging
(
-- * Configuration
  initializeLogging
, initializeFileLogging
)
where

import System.IO

import System.Log.Logger
import System.Log.Handler.Simple
-- import System.Log.Handler.Syslog




-- ----------------------------------------------------------------------------
-- Configuration
-- ----------------------------------------------------------------------------
-- | configures the logging-parameters for the Holumbus-Framework
initializeFileLogging :: FilePath -> [(String, Priority)] -> IO ()
initializeFileLogging file ls = do

    
    handle <- openFile file AppendMode
    -- log all verbose
    v <- verboseStreamHandler handle DEBUG
    updateGlobalLogger "Holumbus" (setLevel WARNING)
    
    updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [v])

    -- set all logLevels for all loggers
    mapM_ (\(s,p) -> updateGlobalLogger s (setLevel p)) ls

    -- log all to syslog
    -- s <- openlog "SyslogStuff" [PID] USER DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s)
    
    -- this will be removed in next versions    
    -- updateGlobalLogger "Holumbus.Network" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.MapReduce.TaskProcessor.task" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.FileSystem.Storage.FileStorage" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.FileSystem.Node.NodeData" (setLevel INFO)
    -- updateGlobalLogger "Holumbus.MapReduce.Types" (setLevel INFO)
    -- updateGlobalLogger "Holumbus.Network.Communication" (setLevel DEBUG)
    -- updateGlobalLogger "Holumbus.MapReduce.JobController.cycle" (setLevel WARNING)

-- | configures the logging-parameters for the Holumbus-Framework
initializeLogging :: [(String, Priority)] -> IO ()
initializeLogging ls = do

    
    -- log all verbose
    v <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [v])

    -- set all logLevels for all loggers
    mapM_ (\(s,p) -> updateGlobalLogger s (setLevel p)) ls

    -- log all to syslog
    -- s <- openlog "SyslogStuff" [PID] USER DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s)
    
    -- this will be removed in next versions
    updateGlobalLogger "Holumbus" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.Network" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.MapReduce.TaskProcessor.task" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.FileSystem.Storage.FileStorage" (setLevel WARNING)
    -- updateGlobalLogger "Holumbus.FileSystem.Node.NodeData" (setLevel INFO)
    -- updateGlobalLogger "Holumbus.MapReduce.Types" (setLevel INFO)
    -- updateGlobalLogger "Holumbus.Network.Communication" (setLevel DEBUG)
    -- updateGlobalLogger "Holumbus.MapReduce.JobController.cycle" (setLevel WARNING)
    
