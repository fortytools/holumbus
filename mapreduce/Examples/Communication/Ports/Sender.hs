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
    
import           Data.List
-- import           Data.Binary

import           Holumbus.Common.Logging
import           Holumbus.Network.Port
import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.Console.Console as Console


version :: String
version = "Ports-Demo Sender 0.1"


main :: IO ()
main
  = do
    initializeLogging []
    putStrLn version
    putStrLn "Begin"
    printStreamController 
    reg <- newPortRegistryFromXmlFile "/tmp/registry.xml"
    setPortRegistry reg
    p <- readPortFromFile "p.port"
    Console.handleUserInput createConsole p

        
createConsole :: Console.ConsoleData (Port String)
createConsole =
  -- Console.addConsoleCommand "send" sendMsg "sends a message" $
  -- Console.addConsoleCommand "sendP" sendPMsg "sends a message to the private port of the receiver" $
  -- Console.addConsoleCommand "sendN" sendNMsg "sends a message to the named port of the receiver" $
  Console.addConsoleCommand "sendG" sendGMsg "sends a message to the global port of the receiver" $
  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" printVersion "prints the version" $ 
  Console.initializeConsole

{-
sendMsg :: StringPort -> [String] -> IO ()
sendMsg p line
  = do
    respStream <- newStringStream
    respPort <- newPortFromStream respStream
    let msg = intercalate " " line
    sendWithGeneric p msg (encode respPort)
    response <- tryWaitReadStreamMsg respStream time10
    case response of
      -- if no response
      Nothing -> putStrLn "no response in 10 seconds"          
      -- handle the response
      (Just r) -> putStrLn $ "ResponseMessage:\n" ++ show r
    closeStream respStream
    
sendPMsg :: StringPort -> [String] -> IO ()
sendPMsg _ line
  = do
    privPort <- (newPort "private" "localhost" 10000)::IO StringPort
    let msg = intercalate " " line
    send privPort msg
 
 
sendNMsg :: StringPort -> [String] -> IO ()
sendNMsg _ line
  = do
    namedPort <- (newPort "named" "localhost" 10001)::IO StringPort
    let msg = intercalate " " line
    send namedPort msg
-}

sendGMsg :: (Port String) -> [String] -> IO ()
sendGMsg _ line
  = do
    globalPort <- (newGlobalPort "global")::IO (Port String)
    let msg = intercalate " " line
    send globalPort msg


printDebug :: (Port String) -> [String] -> IO ()
printDebug _ _
  = do
    printStreamController




printVersion :: (Port String) -> [String] -> IO ()
printVersion _ _
  = do
    putStrLn version
