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
import           Holumbus.Distribution.DChan
import           Holumbus.Common.Logging

import           MessagesDChan



data ServerData = ServerData {
    sd_clients   :: Map.Map ClientToken (String, (DChan ChatResponse))
  , sd_names     :: Set.Set String
  , sd_number    :: Integer
  }


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ (defaultDNodeConfig "ServerDChan")
      { dnc_MinPort = (fromInteger 7999), dnc_MaxPort = (fromInteger 7999) }
    let serverdata = mkNewServer
    channel <- newDChan "server"
    forkIO $ dispatcher serverdata channel
    repeatTillReturn
    closeDChan channel
    deinitDNode

    

mkNewServer :: ServerData
mkNewServer = ServerData Map.empty Set.empty 1


mkNextClientToken :: ServerData -> IO (ServerData, ClientToken)
mkNextClientToken s
  = do
    let i  = sd_number s
        s' = s { sd_number = (i+1)}
    t <- genClientToken i
    return (s', t)

    
addClient :: ServerData -> ClientToken -> String -> DChan ChatResponse -> ServerData
addClient s ct cn cc = s { sd_clients = c' , sd_names = n' }
  where
  c' = Map.insert ct (cn,cc) (sd_clients s)
  n' = Set.insert cn (sd_names s)


deleteClient :: ServerData -> ClientToken -> ServerData
deleteClient s ct = s { sd_clients = c' , sd_names = n' }
  where
  c' = Map.delete ct (sd_clients s)
  n' = case (Map.lookup ct (sd_clients s)) of
          (Just (cn,_)) -> Set.delete cn (sd_names s)
          (Nothing)     -> (sd_names s)

isTokenValid :: ServerData -> ClientToken -> Bool
isTokenValid s ct = Map.member ct (sd_clients s) 

    
isNameValid :: ServerData -> String -> Bool
isNameValid _ "" = False
isNameValid s cn = (not $ Set.member cn (sd_names s))


sendMessage :: ServerData -> ClientToken -> String -> IO ()
sendMessage s ct text
  = do
    if (isTokenValid s ct)
      then do
        let (cn, _) = fromJust $ Map.lookup ct (sd_clients s)
            ccs = map (snd) $ Map.elems $ Map.delete ct (sd_clients s)
        mapM (\cc -> writeDChan cc (RspMessage cn text)) ccs
        return () 
      else return ()
      

repeatTillReturn :: IO ()
repeatTillReturn
  = do
    hnd <- getDNodeData
    putStrLn $ show hnd
    l <- getLine
    case l of
      "exit" -> 
        return ()
      "debug" ->
        do
        nd <- getDNodeData
        putStrLn $ show nd
      _  ->
        repeatTillReturn

            
dispatcher :: ServerData -> DChan ChatRequest -> IO ()
dispatcher s c
  = do
    msg <- readDChan c
    s' <- case msg of
      (ReqRegister cn cc) ->
        do
        if (isNameValid s cn)
          then do
            (s', ct) <- mkNextClientToken s 
            writeDChan cc (RspRegister ct)
            return $ addClient s' ct cn cc
          else do
            writeDChan cc (RspError "name already used")
            return s
      (ReqUnregister ct) -> 
        do
        return $ deleteClient s ct 
      (ReqMessage ct text) ->
        do
        sendMessage s ct text
        return s
    dispatcher s' c    
      
