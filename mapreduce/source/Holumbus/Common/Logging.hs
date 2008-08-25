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
initializeLogging :: IO ()
initializeLogging = do

    -- log all verbose	
    v <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [v])

    updateGlobalLogger "Holumbus" (setLevel DEBUG)
    updateGlobalLogger "Holumbus.Network" (setLevel WARNING)
    updateGlobalLogger "Holumbus.Network.Communication" (setLevel DEBUG)
    updateGlobalLogger "Holumbus.MapReduce.JobController.cycle" (setLevel WARNING)

    -- log all to syslog
    -- s <- openlog "SyslogStuff" [PID] USER DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s)