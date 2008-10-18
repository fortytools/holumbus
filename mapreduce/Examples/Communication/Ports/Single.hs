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

import           Data.Binary
import qualified Data.ByteString.Lazy as B

import           Holumbus.Common.Logging
import           Holumbus.Network.Port


version :: String
version = "Holumbus-Ports Single-Demo 0.1"


type StringStream = Stream String
type StringPort   = Port String


decodeMaybe :: (Binary a) => Maybe B.ByteString -> Maybe a
decodeMaybe Nothing = Nothing
decodeMaybe (Just b) = (Just $ decode b)


decodeStringPort :: Maybe B.ByteString -> Maybe StringPort
decodeStringPort = decodeMaybe



main :: IO ()
main
  = do
    initializeLogging
    putStrLn version
    
    putStrLn ""
    putStrLn "opening private Streams and Ports"
    
    printStreamController
    stream1 <- (newLocalStream (Just "stream1")):: IO StringStream
    stream2 <- (newLocalStream (Just "stream2")):: IO StringStream
    printStreamController
    
    testCase stream1 stream2 "Hello World"
    
    putStrLn ""
    putStrLn "closing streams..."
    closeStream stream1
    closeStream stream2
    printStreamController
    
    
    
testCase :: StringStream -> StringStream -> String -> IO ()
testCase stream1 stream2 t
  = do
    putStrLn ""
    putStrLn "Begin"

    port1   <- newPortFromStream stream1
    port2   <- newPortFromStream stream2
    
    putStrLn ""
    putStrLn $ "Sending: " ++ t
    sendWithGeneric port1 t (encode port2)
    printStreamController

    putStrLn ""
    putStrLn "Reading result..."
    msg1 <- readStreamMsg stream1
    putStrLn "Result:"
    putStrLn $ "Text: " ++ getMessageData msg1
    putStrLn $ "Msg: " ++ show msg1
    
    putStrLn ""
    putStrLn "echoing result..."
    let replyPort = decodeStringPort $ getGenericData msg1
    let text      = getMessageData msg1
    case replyPort of    
        (Nothing) ->
          do
          putStrLn "no reply port in message"
        (Just p) ->
          do
          send p ("ECHO:" ++ text)
    

    putStrLn "Reading echo..."
    msg2 <- readStreamMsg stream2
    putStrLn "Result:"
    putStrLn $ "Text: " ++ getMessageData msg2
    putStrLn $ "Msg: " ++ show msg2
        
    putStrLn ""
    putStrLn "End"
