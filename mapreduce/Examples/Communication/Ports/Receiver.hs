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

import           Control.Concurrent
import qualified Data.ByteString.Lazy as B

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils
import           Holumbus.Network.Port
import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.Console.Console as Console


type StringStream = Stream String
type StringPort   = Port String


newStringStream :: IO StringStream
newStringStream = newLocalStream Nothing


decodeStringPort :: Maybe B.ByteString -> Maybe StringPort
decodeStringPort = decodeMaybe


version :: String
version = "Ports-Demo Receiver 0.1"


main :: IO ()
main
  = do
    initializeLogging
    putStrLn version    
    putStrLn "Begin"
    reg <- newPortRegistryFromXmlFile "/tmp/registry.xml"
    setPortRegistry reg
    printStreamController
    putStrLn "initialising streams..."
    s <- newStringStream
    p <- newPortFromStream s
    globalS <- newStream  STGlobal  (Just "global")  (Just 10002)
    putStrLn "streams initialised"
    writePortToFile p "p.port"
    forkIO $ printMessages "global" globalS
    Console.handleUserInput createConsole ()


printMessages :: String -> StringStream -> IO ()
printMessages info s
  = do
    msg <- readStreamMsg s
    putStrLn $ "port: " ++ info
    putStrLn "Incoming Message:"
    putStrLn $ show msg
    let replyPort = decodeStringPort $ getGenericData msg
    let text      = getMessageData msg
    case replyPort of
        (Nothing) ->  
          do
          putStrLn "no reply port in message"
        (Just p) ->
          do
          send p ("ECHO:" ++ text)
    printMessages info s
    
    
createConsole :: Console.ConsoleData ()
createConsole =
  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" printVersion "prints the version" $ 
  Console.initializeConsole

 
printDebug :: () -> [String] -> IO ()
printDebug _ _
  = do
    printStreamController
  
printVersion :: () -> [String] -> IO ()
printVersion _ _
  = do
    putStrLn version

