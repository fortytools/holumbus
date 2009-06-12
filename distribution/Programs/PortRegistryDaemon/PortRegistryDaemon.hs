-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.PortRegistryDaemon
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides the PortRegistryDeamonm, which is the Portregistry and a server console.

  Usage: PortRegistryDaemon PortRegistryPort ConsolePort Logfile

-}

-- ----------------------------------------------------------------------------

module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Common.FileHandling
import           Holumbus.Common.Utils
import           Holumbus.Network.Port
import           Holumbus.Network.PortRegistry
import           Holumbus.Network.PortRegistry.PortRegistryData
import qualified Holumbus.Console.ServerConsole as Console
import           System.Environment (getArgs)
import           System.Log.Logger
import           System.Exit

version :: String
version = "Holumbus-PortRegistryDaemon 0.1"

prompt :: String
prompt = "# PortRegistryDaemon > "

localLogger :: String
localLogger = "Holumbus.PortRegistryDaemon"

pUsage :: IO ()
pUsage = do
  putStrLn "Usage: PortRegistryDaemon PortRegistryPort ConsolePort Logfile"

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
  = handleAll (\e -> errorM localLogger $ "EXCEPTION: " ++ show e) $ do
    (s_regport:s_cport:logfile:[]) <- params
    initializeFileLogging logfile [(localLogger, INFO),("Holumbus.Network.DoWithServer",INFO)]
    let sn      = "portregistry"
    let regport = Just . fromInteger . read $ s_regport
    let cport   =                      read $ s_cport
    r <- newPortRegistryData sn regport
    p <- getPortRegistryRequestPort r
    writePortToFile p "/tmp/registry.port"
    saveToXmlFile "/tmp/registry.xml" p
    Console.startServerConsole createConsole r cport prompt

    
createConsole :: Console.ConsoleData PortRegistryData
createConsole =
  Console.addConsoleCommand "register"   hdlRegister   "registers a port manually" $
  Console.addConsoleCommand "unregister" hdlUnregister "unregisters a port manually" $
  Console.addConsoleCommand "lookup"   hdlLookup     "gets the socket id for a port" $
  Console.addConsoleCommand "ports"    hdlGetPorts   "lists all ports" $ 
  Console.addConsoleCommand "version"    hdlVersion    "prints the version" $
  Console.initializeConsole
      

hdlRegister :: PortRegistryData -> [String] -> IO String
hdlRegister prd opts
  = do
    handleAll (\_ -> return "usage: register <streamname> <hostname> <socketId>") $
      do
      let sn = head opts
      let hn = head $ tail opts
      let po = fromInteger . read . head . tail . tail $ opts
      registerPort sn (SocketId hn po) prd
      return "Register succesfull"


hdlUnregister :: PortRegistryData -> [String] -> IO String
hdlUnregister prd opts
  = handleAll (\_ -> return "usage: unregister <streamname>") $
      do
      let sn = head opts
      unregisterPort sn prd
      return "Unregister succesfull"


hdlLookup :: PortRegistryData -> [String] -> IO String
hdlLookup prd opts
  = handleAll (\_ -> return "usage: lookup <streamname>") $
      do
      let sn = head opts
      soid <- lookupPort sn prd
      return . show $ soid


hdlGetPorts :: PortRegistryData -> [String] -> IO String
hdlGetPorts prd _
  = do
    ls <- getPorts prd
    return . show $ ls


hdlVersion :: PortRegistryData -> [String] -> IO String
hdlVersion _ _
  = return version

-- ------------------------------------------------------------