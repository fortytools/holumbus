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
import           Data.Binary

import           Holumbus.Network.Port
import qualified Holumbus.Console.Console as Console

type StringStream = Stream String
type StringPort   = Port String

newStringStream :: IO StringStream
newStringStream = newStream

version :: String
version = "Ports-Demo Sender 0.1"

main :: IO ()
main
  = do
    putStrLn version
    putStrLn "Begin"
    printStreamController
    p <- readPortFromFile "p.port"
    Console.handleUserInput createConsole p

        
createConsole :: Console.ConsoleData StringPort
createConsole =
  Console.addConsoleCommand "send" sendMsg "sends a message" $
  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" printVersion "prints the version" $ 
  Console.initializeConsole


sendMsg :: StringPort -> [String] -> IO ()
sendMsg p line
  = do
    respStream <- newStringStream
    respPort <- newPort respStream
    let msg = intercalate " " line
    sendWithGeneric p msg (encode respPort)
    response <- tryWaitReadStreamMsg respStream time10
    case response of
      -- if no response
      Nothing -> putStrLn "no response in 10 seconds"          
      -- handle the response
      (Just r) -> putStrLn $ "ResponseMessage:\n" ++ show r
    closeStream respStream

 
printDebug :: StringPort -> [String] -> IO ()
printDebug _ _
  = do
    printStreamController

  
printVersion :: StringPort -> [String] -> IO ()
printVersion _ _
  = do
    putStrLn version
