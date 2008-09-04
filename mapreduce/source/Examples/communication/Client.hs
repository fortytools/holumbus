
module Main(main) where

import           Control.Exception

import           Holumbus.Common.Debug
import           Holumbus.Common.Logging
import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.Network.Communication


version :: String
version = "Holumbus-Communication Client 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging
      p <- newPortRegistryFromXmlFile "registry.xml"
      setPortRegistry p
      client <- initializeData
      waitForInput client
      deinitializeData client


waitForInput :: Client -> IO ()
waitForInput client
  = do
    line <- getLine
    case line of
      "exit"  -> 
        return ()
      "debug" -> 
        do
        printDebug client
        waitForInput client
      _ -> 
        waitForInput client


initializeData :: IO (Client)
initializeData 
  = do
    client <- newClient "server" Nothing
    return client


deinitializeData :: Client -> IO ()
deinitializeData client
  = do
    closeClient client
