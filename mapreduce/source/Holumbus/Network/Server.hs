-- ----------------------------------------------------------------------------

{- |
  Module     : Server
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Server-Module for the Holumbus framework.

-}

-- ----------------------------------------------------------------------------


module Holumbus.Network.Server
(
-- * running the server in it's own thread
--  startServer
--, stopServer
--, StopException(..)

-- * starting a socket without specific port
  startSocket

-- * server with no thread control
, listenForRequests
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad

--import Data.Typeable

import Network
import qualified Network.Socket as Socket
--import qualified Network.BSD as BSD

import System.Log.Logger
import System.CPUTime
import System.IO
import System.Posix

import Text.Printf

-- ^ logger
localLogger :: String
localLogger = "Holumbus.Server"


type ServerDispatcher = Handle -> HostName -> PortNumber -> IO ()


-- | starts the server in a new thread
{-startServer :: MVar (Maybe ThreadId) -> ServerDispatcher -> PortID -> IO (Bool)
startServer mVarTId sd port
  = do
    threadId <- takeMVar mVarTId
    (r, newThreadId) <- case threadId of
      Nothing -> do 
                 infoM localLogger "starting server"
                 i <- forkIO $ listenForRequests sd port
                 return (True, Just i)
      Just i  -> do 
                 infoM localLogger "server already started"
                 return (False, Just i)
    putMVar mVarTId newThreadId
    return r
-}

-- | stops the server
{- stopServer :: MVar (Maybe ThreadId) -> IO (Bool)
stopServer mVarTId
  = do
    threadId <- takeMVar mVarTId
    (r, newThreadId) <- case threadId of
      Nothing -> do 
                 infoM localLogger "no server running"
                 return (False, Nothing)
      Just i  -> do 
                 infoM localLogger "stopping server"
                 --TODO this does not close the socket... try throwTo
                 killThread i
                 return (True, Nothing)
    putMVar mVarTId newThreadId
    return r
-}

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
            catchDyn (waitForRequests f so) (handler so)
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

-- | This function represents the server side. It opens a socket on the provided port and 
-- starts listening for requests. A post-processing hook is provided to allow some actions on
-- the processing results, e.g. some logging functionality.
listenForRequests :: ServerDispatcher -> PortID -> IO ()
listenForRequests f sid =
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  installHandler sigPIPE Ignore Nothing
  socket <- listenOn sid
  waitForRequests f socket

waitForRequests :: ServerDispatcher -> Socket -> IO ()
waitForRequests f socket = 
  do
  client <- accept socket
  forkIO $ processRequest f client   -- Spawn new thread to answer the current request.
  waitForRequests f socket           -- Wait for more requests.

processRequest :: ServerDispatcher -> (Handle, HostName, PortNumber) -> IO ()
processRequest f client = 
  bracket (return client) (\(hdl, _, _) -> hClose hdl) (\cl -> processRequest' cl)
    where
    processRequest' (hdl, hst, prt) = 
      do
      hSetBuffering hdl NoBuffering
      -- Dispatch the request and measure the processing time.
      t1 <- getCPUTime
      debugM localLogger "starting to dispatch request"
      handle (\_ -> errorM localLogger "UnknownError") $ do
        f hdl hst prt
      t2 <- getCPUTime
      d <- return ((fromIntegral (t2 - t1) / 1000000000000) :: Float)
      ds <- return (printf "%.4f" d)
      infoM localLogger ("request processed in " ++ ds ++ " sec")
