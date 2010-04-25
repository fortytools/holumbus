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

import           Control.Concurrent
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set

import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DFunction
import           Holumbus.Common.Logging

import           MessagesDChan



data ServerData = ServerData {
    sd_clients   :: Map.Map Int (String, RemoteClientInterface)
  , sd_names     :: Set.Set String
  , sd_number    :: Int
  }

type Server = MVar ServerData


registerClient :: Server -> RegisterClientFunction
registerClient _ "" _ = return Nothing
registerClient cs cn stub
  = do
    putStrLn $ "registering: " ++ cn
    server <- takeMVar cs
    if (Set.member cn (sd_names server))
      then do
        putMVar cs server
        return Nothing
      else do
        let sd_clients' = Map.insert (sd_number server) (cn, createRemoteClientInterface stub) (sd_clients server)
            sd_names'   = Set.insert cn (sd_names server)
            sd_number'  = 1 + (sd_number server)
        putMVar cs $ server { sd_clients = sd_clients', sd_names = sd_names', sd_number = sd_number' }
        return (Just $ sd_number server)


unregisterClient :: Server -> UnregisterClientFunction
unregisterClient cs cId
  = do
    putStrLn $ "unregistering: " ++ (show cId)
    server <- takeMVar cs
    case (Map.lookup cId (sd_clients server)) of
      Nothing -> do
        putMVar cs server
        return ()
      (Just (cn,_)) -> do
        let sd_clients' = Map.delete cId (sd_clients server)
            sd_names'   = Set.delete cn (sd_names server)
        putMVar cs $ server { sd_clients = sd_clients', sd_names = sd_names' }


sendChatMessage :: Server -> SendChatMessageFunction
sendChatMessage cs cId msg
  = do
    putStrLn $ "sending: " ++ (show cId) ++ "> " ++ msg
    server <- readMVar cs
    case (Map.lookup cId (sd_clients server)) of
      Nothing -> do
        -- TODO return true / false to tell the client that his request was accepted or not
        putStrLn $ "warning: unnkown client with id " ++ (show cId) ++ " wants to send message: " ++ msg
        return ()
      (Just (cn,_)) -> do
        -- TODO exception handling here
        let funs = map (\t -> (cif_receive $ snd t) cn msg) $ Map.elems $ Map.filterWithKey (\cId' _ -> cId' /= cId) (sd_clients server)
        sequence_ funs


mkChatServer :: IO Server
mkChatServer = newMVar $ ServerData Map.empty Set.empty 1


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ (defaultDNodeConfig "ChatServer")
      { dnc_MinPort = (fromInteger 7999), dnc_MaxPort = (fromInteger 7999) }
    cs <- mkChatServer
    sifs <- createLocalServerInterfaceStub
      (registerClient cs)
      (unregisterClient cs)
      (sendChatMessage cs)
    inputLoop
    closeLocalServerInterfaceStub sifs
    deinitDNode


inputLoop :: IO ()
inputLoop
  = do
    msg <- getLine
    case msg of
      "exit" -> return ()
      _ -> inputLoop

