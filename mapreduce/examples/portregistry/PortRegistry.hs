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

import           Control.Exception

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils
import           Holumbus.Network.Port
import           Holumbus.Network.PortRegistry
import           Holumbus.Network.PortRegistry.PortRegistryData
import qualified Holumbus.Console.Console as Console


version :: String
version = "Holumbus-PortRegistry 0.1"


main :: IO ()
main
  = do
    initializeLogging
    let sn = "portregistry"
    let po = (Just 9000)
    r <- newPortRegistryData sn po
    p <- getPortRegistryRequestPort r
    writePortToFile p "registry.port"
    saveToXmlFile "registry.xml" p
    Console.handleUserInput createConsole r 
    putStrLn version

    
createConsole :: Console.ConsoleData PortRegistryData
createConsole =
  Console.addConsoleCommand "register" hdlRegister "registers a port manually" $
  Console.addConsoleCommand "unregister" hdlUnregister "unregisters a port manually" $
  Console.addConsoleCommand "lookup" hdlLookup "gets the socket id for a port" $
  Console.addConsoleCommand "ports" hdlGetPorts "lists all ports" $ 
  Console.addConsoleCommand "version" hdlVersion "prints the version" $ 
  Console.initializeConsole


hdlRegister :: PortRegistryData -> [String] -> IO ()
hdlRegister prd opts
  = do
    handle (\_ -> putStrLn "usage: register <streamname> <hostname> <socketId>") $
      do
      let sn = head opts
      let hn = head $ tail opts
      let po = fromInteger $ read $ head $ tail $ tail opts
      registerPort sn (SocketId hn po) prd


hdlUnregister :: PortRegistryData -> [String] -> IO ()
hdlUnregister prd opts
  = handle (\_ -> putStrLn "usage: unregister <streamname>") $
      do
      let sn = head opts
      unregisterPort sn prd


hdlLookup :: PortRegistryData -> [String] -> IO ()
hdlLookup prd opts
  = handle (\_ -> putStrLn "usage: lookup <streamname>") $
      do
      let sn = head opts
      soid <- lookupPort sn prd
      putStrLn $ show soid


hdlGetPorts :: PortRegistryData -> [String] -> IO ()
hdlGetPorts prd _
  = do
    ls <- getPorts prd
    putStrLn $ show ls


hdlVersion :: PortRegistryData -> [String] -> IO ()
hdlVersion _ _
  = do
    putStrLn version
    