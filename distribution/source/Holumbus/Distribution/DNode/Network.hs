-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DNode.Network
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Server-Module for the Holumbus framework.
  
  It contains the lowlevel functions, like the socket handling (opening, 
  reading, writing, ...).
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Distribution.DNode.Network
    (
      -- * Server-Datatypes
      SocketServer
    , getSocketServerName
    , getSocketServerPort
    
    , HandlerFunction
    
    -- * Server-Operations
    , startSocketServer
    , stopSocketServer

    -- * Client-Operations
    , performUnsafeSendRequest
    , performSafeSendRequest    
    , performMaybeSendRequest

    -- * Handle-Operations
    , putByteStringMessage
    , getByteStringMessage
    )
where

import           Prelude hiding         ( catch )

import           Control.Concurrent
import           Control.Exception      (
            Exception
          , IOException
          , bracket
          , catch
          )

import qualified Data.ByteString.Lazy   as B
import           Data.Typeable

import           Network
import qualified Network.Socket         as Socket

import           System.Log.Logger
import           System.CPUTime
import           System.IO
import           System.Posix

import           Text.Printf

import           Holumbus.Common.Utils (handleAll)


-- | Logger
localLogger :: String
localLogger = "Holumbus.Distribution.DNode.Network"


-- ----------------------------------------------------------------------------
-- exception stuff
-- ----------------------------------------------------------------------------

-- | This exception is needed to stop the socket server threads in a controlled
--   way and to distinguish this kind of exception from IOExceptions.
data SocketServerException = SocketServerException ThreadId
  deriving (Typeable, Show)

instance Exception SocketServerException where


-- ----------------------------------------------------------------------------
-- Server-Datatypes
-- ----------------------------------------------------------------------------

data SocketServer = SocketServer { 
    ss_ThreadId    :: ! ThreadId
  , ss_HostName    :: ! HostName
  , ss_PortNumber  :: ! PortNumber
  } deriving (Show)

getSocketServerName :: SocketServer -> HostName
getSocketServerName = ss_HostName

getSocketServerPort :: SocketServer -> PortNumber
getSocketServerPort = ss_PortNumber


type HandlerFunction a = Handle -> IO a


-- ----------------------------------------------------------------------------
-- Server-Operations
-- ----------------------------------------------------------------------------

-- | Creates a new (unix-)socket and starts the listener in its own thread.
--   You'll get a reference to the listener Thread, so you can kill it with
--   stopSocketServer.
--   It is also possible to give a range of PortNumbers on which the socket
--   will be opened. The first portnumber available will be taken.
startSocketServer
  :: HandlerFunction () -- ^ dispatcher function
  -> Int                -- ^ start port number
  -> Int                -- ^ end port number
  -> IO (Maybe SocketServer)
startSocketServer f actPo maxPo
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
          handleAll
            (\e ->
              do
              putStrLn $ "ERROR - socket closed with exception: " ++ show e 
              sClose so
            ) $
            do
            {- 6.8
            catchDyn (waitForRequests f so (SocketId hn po)) (handler so)
            -}
            catch
              (waitForRequests f so)
              (handler so)
        return (Just $ SocketServer tid hn po)
    where
    handler :: Socket -> SocketServerException -> IO ()
    handler so (SocketServerException i)
        = do
          sClose so
          putStrLn $ "socket normally closed by thread " ++ show i 


-- | Stops a socker server by its internal thread id.
stopSocketServer :: SocketServer -> IO ()
stopSocketServer ss
  = do
    let i = ss_ThreadId ss
    me <- myThreadId
    debugM localLogger $ "stopping socket server... with threadId: " ++ show i ++ " - form threadId: " ++ show me
    throwTo i (SocketServerException me)
    yield


-- | Gets the hostname of the computer of just "localhost".
getHostName :: IO (HostName)
getHostName
  = do
    (hn, _) <- Socket.getNameInfo [] True False (Socket.SockAddrUnix "localhost")
    return (maybe "localhost" id hn)


-- | Gets the first free port number and creates of new socket on it.
getFirstSocket :: Int -> Int -> IO (Maybe (Socket, PortNumber))
getFirstSocket actPo maxPo
  = do
    if (actPo > maxPo)
      then do
        return Nothing 
      else do
        handleAll (return (getFirstSocket (actPo+1) maxPo)) $
          do 
          debugM localLogger $ "getFirstSocket: getting Socket for: " ++ show actPo
          socket <- getSocket $ fromIntegral actPo
          return (Just (socket, fromIntegral actPo))     


-- | Opens a socket on a port number.
getSocket :: PortNumber -> IO (Socket)
getSocket po =
  -- for MS-Windows Systems
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  installHandler sigPIPE Ignore Nothing
  socket <- listenOn (PortNumber po)
  return socket


-- | Listens to a socket and opens a new dispatcher thread for every incomming
--   data.
waitForRequests :: HandlerFunction () -> Socket -> IO ()
waitForRequests f socket = 
  do
  (hdl,_,_) <- accept socket
  forkIO $ processRequest f hdl  -- Spawn new thread to answer the current request.
  waitForRequests f socket       -- Wait for more requests.


-- | A wrapper around the user defined dispatcher function.
--   Mesures the time and catches unhandled exceptions.
processRequest :: HandlerFunction () -> Handle -> IO ()
processRequest f conn = 
  bracket (return conn) (hClose) (processRequest')
    where
    processRequest' hdl = 
      do
      hSetBuffering hdl $ BlockBuffering Nothing
      -- Dispatch the request and measure the processing time.
      t1 <- getCPUTime
      debugM localLogger "starting to dispatch request"
      handleAll (\e -> errorM localLogger $ "UnknownError: " ++ show e) $ f hdl
      t2 <- getCPUTime
      d <- return ((fromIntegral (t2 - t1) / 1000000000000) :: Float)
      ds <- return (printf "%.4f" d)
      infoM localLogger ("request processed in " ++ ds ++ " sec")


-- ----------------------------------------------------------------------------
-- Client-Operations
-- ----------------------------------------------------------------------------

    
-- | Send the query to a server and merge the result with the global result.
sendRequest :: HandlerFunction a -> HostName -> PortNumber -> IO a
sendRequest f n p = 
  withSocketsDo $ do 
    installHandler sigPIPE Ignore Nothing
    
    --TODO exception handling
    --handle (\e -> do putStrLn $ show e return False) $
    bracket (connectTo n (PortNumber p)) (hClose) (send)
    where    
    send hdl 
      = do
        hSetBuffering hdl $ BlockBuffering Nothing
        f hdl 


-- no exception handling
performUnsafeSendRequest :: HandlerFunction a -> HostName -> PortNumber -> IO a
performUnsafeSendRequest = sendRequest


-- all IOExceptions handled return of default value
performSafeSendRequest :: HandlerFunction a -> a -> HostName -> PortNumber -> IO a
performSafeSendRequest f d n p
  = catch (sendRequest f n p)
          (\(e ::IOException) -> 
            do
            debugM localLogger $  show e 
            return d)


-- all IOExceptions handled return of Nothing
performMaybeSendRequest :: HandlerFunction a -> HostName -> PortNumber -> IO (Maybe a)
performMaybeSendRequest f n p
  = catch (do
           res <- sendRequest f n p
           return (Just res))
          (\(e ::IOException) -> 
            do
            debugM localLogger $  show e 
            return Nothing)


-- ----------------------------------------------------------------------------
-- Handle-Operations
-- ----------------------------------------------------------------------------

-- | Puts a bytestring to a handle. But to make the reading easier, we write
--   the length of the data as a message-header to the handle, too. 
putByteStringMessage :: B.ByteString -> Handle -> IO ()
putByteStringMessage msg hdl
  = do
    handleAll (\e -> do
      errorM localLogger $ "putMessage: " ++ show e
      errorM localLogger $ "message: " ++ show msg 
     ) $ do
      hPutStrLn hdl ((show $ B.length msg) ++ " ")
      B.hPut hdl msg
      hFlush hdl


-- | Reads data from a stream. We define, that the first line of the message
--   is the message header which tells us how much bytes we have to read.
getByteStringMessage :: Handle -> IO (B.ByteString)
getByteStringMessage hdl
  = do
    line <- hGetLine hdl
    let pkg = words line
    raw <- B.hGet hdl (read $ head pkg)
    return raw