
module Main(main) where

import           Control.Exception

import           Holumbus.Common.Debug
import           Holumbus.Common.Logging
import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.Network.Communication


version :: String
version = "Holumbus-Communication Server 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p
      server <- initializeData
      waitForInput server
      deinitializeData server


waitForInput :: Server -> IO ()
waitForInput server
  = do
    line <- getLine
    case line of
      "exit"  -> 
        return ()
      "debug" -> 
        do
        printDebug server
        waitForInput server
      _ -> 
        waitForInput server


dispatch 
  :: String 
  -> IO (Maybe String)
dispatch msg
  = do
    putStrLn $ "message arrived: " ++ msg
    return Nothing


register :: IdType -> ClientPort -> IO ()
register i cp
  = do
    putStrLn $ "register request: Id: " ++ show i ++ " -- Port: " ++ show cp


unregister :: IdType -> ClientPort -> IO ()
unregister i cp
  = do
    putStrLn $ "unregister request: Id: " ++ show i ++ " -- Port: " ++ show cp


initializeData :: IO (Server)
initializeData 
  = do
    server <- newServer "server" Nothing dispatch (Just register) (Just unregister)
    return server


deinitializeData :: Server -> IO ()
deinitializeData server
  = do
    closeServer server
