
module Holumbus.Common.Logging where

import System.IO

import System.Log.Logger
import System.Log.Handler.Simple
-- import System.Log.Handler.Syslog

initializeLogging :: IO ()
initializeLogging = do

    -- log all verbose	
    v <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [v])

    -- log all to syslog
    -- s <- openlog "SyslogStuff" [PID] USER DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s)