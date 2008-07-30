-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.Core
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Server-Module for the Holumbus framework.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Holumbus.Network.Core
(
-- * Socket-Descriptor
  SocketId(..)

-- * Server-Operations
, startSocket

-- server with no thread control
-- , listenForRequests

-- * Client-Operations
, sendRequest


-- * Handle-Operations
, putMessage
, getMessage
)
where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           Network
import qualified Network.Socket as Socket
import           System.Log.Logger
import           System.CPUTime
import           System.IO
import           System.Posix
import           Text.Printf

import           Text.XML.HXT.Arrow



-- | logger
localLogger :: String
localLogger = "Holumbus.Network.Core"


type ServerDispatcher = SocketId -> Handle -> HostName -> PortNumber -> IO ()



-- ----------------------------------------------------------------------------
-- Socket Descriptor
-- ----------------------------------------------------------------------------

-- | All data, that is needed to address a socket.
--   Contains the hostname and the portNumber
data SocketId = SocketId HostName PortNumber 
  deriving (Show, Eq)

instance Binary (SocketId) where
  put (SocketId hn po)
    = put hn >> (put . toInteger) po
  get
    = do
      hn <- get
      poInt <- get
      return (SocketId hn (fromInteger poInt))

instance XmlPickler SocketId where
  xpickle = xpSocketId

xpSocketId :: PU SocketId
xpSocketId
  = xpElem "socket" $
    xpWrap(\(hn, po) -> SocketId hn (fromInteger po), \(SocketId hn po) -> (hn, (toInteger po))) $
    xpPair (xpAttr "hostname" xpText) (xpAttr "port" xpickle)




-- ----------------------------------------------------------------------------
-- Server-Operations
-- ----------------------------------------------------------------------------


startSocket :: ServerDispatcher -> PortNumber -> PortNumber -> IO (Maybe (ThreadId, HostName, PortNumber))
startSocket f actPo maxPo
  = do
    s <- (getFirstSocket actPo maxPo)
    case s of
      Nothing -> 
        return Nothing
      (Just (so, po)) ->
        do
        hn <- getHostName
        tid <- forkIO $ 
          do
          handle
            (\e ->
              do
              putStrLn $ "ERROR - socket closed with exception: " ++ show e 
              sClose so
            ) $
            do
            catchDyn (waitForRequests f so (SocketId hn po)) (handler so)
        return (Just (tid, hn, po))
    where
      handler :: Socket -> ThreadId -> IO ()
      handler so i 
        = do
          sClose so
          putStrLn $ "socket normally closed by thread " ++ show i 


getHostName :: IO (HostName)
getHostName
  = do
    (hn, _) <- Socket.getNameInfo [] True False (Socket.SockAddrUnix "localhost")
    return (maybe "localhost" id hn)


getFirstSocket :: PortNumber -> PortNumber -> IO (Maybe (Socket, PortNumber))
getFirstSocket actPo maxPo
  | actPo > maxPo = return Nothing
  | otherwise 
    = do
      handle (return (getFirstSocket (actPo+1) maxPo)) $
        do
        socket <- getSocket (PortNumber actPo)
        return (Just (socket, actPo))     


getSocket :: PortID -> IO (Socket)
getSocket po =
  -- for MS-Windows Systems
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  installHandler sigPIPE Ignore Nothing
  socket <- listenOn po
  return socket

{-
-- | This function represents the server side. It opens a socket on the provided port and 
-- starts listening for requests. A post-processing hook is provided to allow some actions on
-- the processing results, e.g. some logging functionality.
listenForRequests :: ServerDispatcher -> PortID -> IO ()
listenForRequests f sid =
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  installHandler sigPIPE Ignore Nothing
  socket <- listenOn sid
  -- TODO
  waitForRequests f socket undefined
-}


waitForRequests :: ServerDispatcher -> Socket -> SocketId -> IO ()
waitForRequests f socket soid = 
  do
  client <- accept socket
  forkIO $ processRequest f soid client   -- Spawn new thread to answer the current request.
  waitForRequests f socket soid       -- Wait for more requests.


processRequest :: ServerDispatcher -> SocketId -> (Handle, HostName, PortNumber) -> IO ()
processRequest f soid client = 
  bracket (return client) (\(hdl, _, _) -> hClose hdl) (\cl -> processRequest' cl)
    where
    processRequest' (hdl, hst, prt) = 
      do
      hSetBuffering hdl NoBuffering
      -- Dispatch the request and measure the processing time.
      t1 <- getCPUTime
      debugM localLogger "starting to dispatch request"
      handle (\e -> errorM localLogger $ "UnknownError: " ++ show e) $ do
        f soid hdl hst prt
      t2 <- getCPUTime
      d <- return ((fromIntegral (t2 - t1) / 1000000000000) :: Float)
      ds <- return (printf "%.4f" d)
      infoM localLogger ("request processed in " ++ ds ++ " sec")

      
      

-- ----------------------------------------------------------------------------
-- Client-Operations
-- ----------------------------------------------------------------------------

    
-- | Send the query to a server and merge the result with the global result.
sendRequest :: (Handle -> IO a) -> HostName -> PortID -> IO a
sendRequest f n p = 
  withSocketsDo $ do 
    installHandler sigPIPE Ignore Nothing
    
    --TODO exception handling
    --handle (\e -> do putStrLn $ show e return False) $
    bracket (connectTo n p) (hClose) (send)
    where    
    send hdl 
      = do
        hSetBuffering hdl NoBuffering
        f hdl



-- ----------------------------------------------------------------------------
-- Handle-Operations
-- ----------------------------------------------------------------------------


putMessage :: B.ByteString -> Handle -> IO ()
putMessage msg hdl
  = do
    handle (\e -> errorM localLogger $ "putMessage: " ++ show e) $ do
      hPutStrLn hdl ((show $ B.length msg) ++ " ")
      B.hPut hdl msg


getMessage :: Handle -> IO (B.ByteString)
getMessage hdl
  = do
    line <- hGetLine hdl
    let pkg = words line
    raw <- B.hGet hdl (read $ head pkg)
    return raw
  