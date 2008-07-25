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
import           Data.Binary
import qualified Data.ByteString.Lazy as B

import           Holumbus.Common.Utils
import           Holumbus.Network.Port
import qualified Holumbus.Console.Console as Console


type StringStream = Stream String
type StringPort   = Port String

newStringStream :: IO StringStream
newStringStream = newStream


decodeMaybe :: (Binary a) => Maybe B.ByteString -> Maybe a
decodeMaybe Nothing = Nothing
decodeMaybe (Just b) = (Just $ decode b)


decodeStringPort :: Maybe B.ByteString -> Maybe StringPort
decodeStringPort = decodeMaybe

version :: String
version = "Ports-Demo Receiver 0.1"


main :: IO ()
main
  = do
    putStrLn version    
    putStrLn "Begin"
    printStreamController
    s <- newStringStream
    p <- newPort s
    writePortToFile p "p.port"
    saveToXmlFile "port.xml" p
    forkIO $ printMessages s
    Console.handleUserInput createConsole ()


printMessages :: StringStream -> IO ()
printMessages s
  = do
    msg <- readStreamMsg s
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
    printMessages s
    
    
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

