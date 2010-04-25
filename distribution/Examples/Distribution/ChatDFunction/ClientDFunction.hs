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

{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Main(main) where

import           Control.Concurrent
import           Control.Exception
import           Data.Typeable
import           System.IO

import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DFunction
import           Holumbus.Common.Logging

import           InterfacesDFunction


data ConnectionException = ConnectionException
  deriving(Typeable, Show)
instance Exception ConnectionException


data ClientData = ClientData {
    cd_DNodeId :: DNodeId
  , cd_id      :: Maybe Int
  , cd_tid     :: ThreadId
  , cd_name    :: String
  , cd_stub    :: ClientInterfaceStub
  , cd_server  :: RemoteServerInterface
  , cd_exit    :: IO ()
  }

type Client = MVar ClientData


-- what to do when we see the server for the first time after a disconnection
positiveHandlerFunction :: Client -> DHandlerId -> IO ()
positiveHandlerFunction cc _
  = do
    putStrLn "positive Trigger"
    client <- takeMVar cc
    clearStdIn
    hFlush stdout
    -- kill the wait thread
    throwTo (cd_tid client) ConnectionException
    -- start the chat thread
    tid <- forkIO $ chatLoop cc
    putMVar cc $ client { cd_tid = tid }


-- what to do when the server becomes unreachable
negativeHandlerFunction :: Client -> DHandlerId -> IO ()
negativeHandlerFunction cc _
  = do
    putStrLn "negative Trigger"
    client <- takeMVar cc
    -- kill the chat thread
    throwTo (cd_tid client) ConnectionException
    -- start the wait thread
    tid <- forkIO $ waitLoop
    putMVar cc $ client { cd_id = Nothing, cd_tid = tid }
    

mkChatClient :: DNodeId -> ThreadId -> ClientInterfaceStub -> RemoteServerInterface -> IO () -> IO Client
mkChatClient myDNodeId tid cifs rsif endfun
  = do
    newMVar $ ClientData myDNodeId Nothing tid "" cifs rsif endfun


clearStdIn :: IO()
clearStdIn
  = do
    b <- hReady stdin
    if b
      then do
        _ <- getChar
        clearStdIn
      else do 
        return ()    


generateExitFunktions :: IO (IO (), IO ())
generateExitFunktions
  = do
    quitMarker <- newEmptyMVar
    return (waitForTermination quitMarker, terminateProgram quitMarker)
    where
    waitForTermination :: MVar () -> IO ()
    waitForTermination qm
      = do
        _ <- takeMVar qm
        return ()
    terminateProgram :: MVar () -> IO ()
    terminateProgram qm
      = do
        putMVar qm ()


waitLoop :: IO ()
waitLoop
  = handle (\ConnectionException -> return ()) $
      do
      putStrLn "waiting for server..."
      hFlush stdout
      threadDelay 10000000
      waitLoop


chatLoop :: Client -> IO ()
chatLoop cc
  = handle (\ConnectionException -> return ()) $
      do
      client <- readMVar cc
      case (cd_id client) of
        (Just cid) -> do
          -- putStr $ (cd_name client) ++ "> "
          hFlush stdout
          msg <- getLine
          case msg of
            "exit" -> do
              (sif_unregister $ cd_server client) cid
              (cd_exit client)
            _ -> (sif_send $ cd_server client) cid msg
        Nothing -> do
          putStr "login with name: "
          hFlush stdout
          cn <- getLine
          mbId <- (sif_register $ cd_server client) cn (cd_DNodeId client) (cd_stub client)
          client' <- takeMVar cc
          putMVar cc $ client' {cd_id = mbId, cd_name = cn}
      chatLoop cc


receiveChatMessage :: ReceiveChatMessageFunction
receiveChatMessage cn msg
  = do
    putStrLn $ "\n" ++ cn ++ "> " ++ msg



main :: IO ()
main
  = do
    initializeLogging []
    myDNodeId <- initDNode $ defaultDNodeConfig ""
    addForeignDNode $ mkDNodeAddress "ChatServer" "april" (fromInteger 7999)
    (waitForTermination, terminateProgram) <- generateExitFunktions
    -- start with the wait loop
    tid <- forkIO waitLoop
    -- create chat client data strukture
    cifs <- createLocalClientInterfaceStub receiveChatMessage
    rsif <- createRemoveServerInterface "ChatServer"
    cc <- mkChatClient myDNodeId tid cifs rsif terminateProgram
    -- register the handlers which will controll the existence of the server
    let dni = mkDNodeId "ChatServer"
    addForeignDNodeHandler True dni (positiveHandlerFunction cc)
    addForeignDNodeHandler False dni (negativeHandlerFunction cc)
    -- wait for the chatLoop to stop by the user
    waitForTermination
    -- clean up
    closeLocalClientInterfaceStub cifs
    deinitDNode


