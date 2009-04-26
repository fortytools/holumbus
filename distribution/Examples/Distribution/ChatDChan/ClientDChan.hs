-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------


module Main(main) where

import           Prelude hiding (catch)

import           System.IO
import           Control.Concurrent
import           Control.Exception

import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DChan
import           Holumbus.Common.Logging

import           Examples.Distribution.ChatDChan.MessagesDChan

data ClientData = ClientData {
    cd_server :: DChan ChatRequest
  , cd_client :: DChan ChatResponse
  , cd_name   :: String
  , cd_token  :: ClientToken
  }


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ defaultDNodeConfig ""
    addForeignDNode $ mkDNodeAddress "ServerDChan" "april" (fromInteger 7999)
    sc <- newRemoteDChan "server" "ServerDChan"
    
    -- addForeignDNodeHandler (mkDNodeId "ServerDChan") listener
    
    cc <- newDChan ""
    cd <- registerClient sc cc
    forkIO $ messageReader cd
    messageWriter cd
    unregisterClient cd
    closeDChan sc
    closeDChan cc
    deinitDNode
    
    
registerClient :: DChan ChatRequest -> DChan ChatResponse -> IO ClientData
registerClient sc cc
  = do
    putStr "please insert login name: "
    hFlush stdout
    cn <- getLine
    handle (\(SomeException e) -> do
      putStr $ show e
      putStrLn "ERROR: server not reachable"
      registerClient sc cc
      ) $
      do 
      writeDChan sc (ReqRegister cn cc)
      rsp <- readDChan cc
      case rsp of
        (RspRegister ct) ->
          return $ ClientData sc cc cn ct
        (RspError e) ->
          do
          putStrLn $ "ERROR: " ++ e
          registerClient sc cc
        _ ->
          do
          putStrLn $ "ERROR: invalid server response" 
          registerClient sc cc


unregisterClient :: ClientData -> IO ()
unregisterClient cd
  = do
    handle (\(SomeException _) -> do return ()) $
      writeDChan (cd_server cd) (ReqUnregister (cd_token cd))


messageReader :: ClientData -> IO ()
messageReader cd
  = do
    msg <- readDChan (cd_client cd)
    case msg of
      (RspMessage cn text) ->
        do
        putStrLn $ cn ++ "> " ++ text
      (RspError e) ->
        do
        putStr $ "ERROR: " ++ e
      _ ->
        do
        putStrLn $ "ERROR: invalid server response" 
    messageReader cd


messageWriter :: ClientData -> IO ()
messageWriter cd
  = do
    putStr $ (cd_name cd) ++ "> "
    hFlush stdout
    msg <- getLine
    case msg of
      "exit" -> 
        return ()
      "debug" ->
        do
        nd <- getDNodeData
        putStrLn $ show nd
        messageWriter cd
      "" ->
        do
        messageWriter cd
      text ->
        do
        handle (\(SomeException _) -> do 
          putStrLn "ERROR: Server unreachable, message not send"
          ) $ do
          writeDChan (cd_server cd) (ReqMessage (cd_token cd) text)
          messageWriter cd