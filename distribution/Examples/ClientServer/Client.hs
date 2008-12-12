
module Main(main) where

import           Holumbus.Common.Debug
import           Holumbus.Common.Logging
import           Holumbus.Common.Utils
import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.Network.Communication

version :: String
version = "Holumbus-Communication Client 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handleAll (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging []
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
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


dispatch 
  :: String 
  -> IO (Maybe String)
dispatch msg
  = do
    putStrLn $ "message arrived: " ++ msg
    return Nothing


initializeData :: IO (Client)
initializeData 
  = do
    client <- newClient "server" Nothing dispatch
    return client


deinitializeData :: Client -> IO ()
deinitializeData client
  = do
    closeClient client
