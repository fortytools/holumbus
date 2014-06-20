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
  
  It contains the lowlevel functions, like the socket handling (opening, 
  reading, writing, ...).
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Network.Core
{-# DEPRECATED "this module will be remove in the next release, please use the packages from Holumbus.Distribution.*" #-}
    (
      -- * Socket-Descriptor
      SocketId(..)

      -- * Server-Operations
    , startSocket

      -- * Client-Operations
      -- deprecated
    , sendRequest
    
      -- use this
    , performUnsafeSendRequest
    , performSafeSendRequest    
    , performMaybeSendRequest

      -- * Handle-Operations
    , putMessage
    , getMessage

    , ThreadIdException(..)
    )
where

import           Prelude hiding         ( catch )

import           Control.Concurrent
import           Control.Exception      ( Exception
          , IOException
          , bracket
          , catch
          )

import           Data.Binary
--import           Holumbus.Common.MRBinary
import qualified Data.ByteString.Lazy   as B
import           Data.Typeable

import           Network
import qualified Network.Socket         as Socket

import           System.Log.Logger
import           System.CPUTime
import           System.IO
import           System.Posix

import           Text.Printf
import           Text.XML.HXT.Core

import           Holumbus.Common.Utils  ( handleAll )

-- | Logger
localLogger :: String
localLogger = "Holumbus.Network.Core"


type ServerDispatcher = SocketId -> Handle -> SocketId -> IO ()

-- ------------------------------------------------------------
--
-- exception stuff

data ThreadIdException  = ThreadIdException ThreadId
                          deriving (Typeable, Show)

instance Exception ThreadIdException where

-- ----------------------------------------------------------------------------
-- Socket Descriptor
-- ----------------------------------------------------------------------------

-- | All data, that is needed to address a socket.
--   Contains the hostname and the portNumber.
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

-- | Creates a new (unix-)socket and starts the listener in its own thread.
--   You'll get the threadId of the listener Thread, so you can kill it.
--   It is also possible to give a range of PortNumbers on which the socket
--   will be opened. The first portnumber available will be taken.
startSocket 
  :: ServerDispatcher -- ^ dispatcher function
  -> PortNumber       -- ^ start port number
  -> PortNumber       -- ^ end port number
  -> IO (Maybe (ThreadId, HostName, PortNumber))
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
              (waitForRequests f so (SocketId hn po))
              (handler so)
        return (Just (tid, hn, po))
    where
    handler :: Socket -> ThreadIdException -> IO ()
    handler so (ThreadIdException i)
        = do
          sClose so
          putStrLn $ "socket normally closed by thread " ++ show i 


-- | Gets the hostname of the computer of just "localhost".
getHostName :: IO (HostName)
getHostName
  = do
    (hn, _) <- Socket.getNameInfo [] True False (Socket.SockAddrUnix "localhost")
    return (maybe "localhost" id hn)


-- | Gets the first free port number and creates of new socket on it.
getFirstSocket :: PortNumber -> PortNumber -> IO (Maybe (Socket, PortNumber))
getFirstSocket actPo maxPo
  = do
    -- due to a bug in Network.Socket, we cannot use the (>) directly for PortNumbers
    let actI = toInteger actPo
        maxI = toInteger maxPo
    if (actI > maxI)
      then do
        return Nothing 
      else do
        handleAll (return (getFirstSocket (actPo+1) maxPo)) $
          do
          debugM localLogger $ "getFirstSocket: getting Socket for: " ++ show actPo
          socket <- getSocket (PortNumber actPo)
          return (Just (socket, actPo))     


-- | Opens a socket on a port number.
getSocket :: PortID -> IO (Socket)
getSocket po =
  -- for MS-Windows Systems
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  _ <- installHandler sigPIPE Ignore Nothing
  socket <- listenOn po
  return socket


-- | Listens to a socket and opens a new dispatcher thread for every incomming
--   data.
waitForRequests :: ServerDispatcher -> Socket -> SocketId -> IO ()
waitForRequests f socket soid = 
  do
  client <- accept socket
  _ <- forkIO $ processRequest f soid client   -- Spawn new thread to answer the current request.
  waitForRequests f socket soid       -- Wait for more requests.


-- | A wrapper around the user defined dispatcher function.
--   Mesures the time and catches unhandled exceptions.
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
      handleAll (\e -> errorM localLogger $ "UnknownError: " ++ show e) $ do
        f soid hdl (SocketId hst prt)
      t2 <- getCPUTime
      d <- return ((fromIntegral (t2 - t1) / 1000000000000) :: Float)
      ds <- return (printf "%.4f" d)
      infoM localLogger ("request processed in " ++ ds ++ " sec")


-- ----------------------------------------------------------------------------
-- Client-Operations
-- ----------------------------------------------------------------------------

    
-- | Send the query to a server and merge the result with the global result.
sendRequest :: (Handle -> IO a) -> HostName -> PortNumber -> IO a
sendRequest f n p = 
  withSocketsDo $ do 
    _ <- installHandler sigPIPE Ignore Nothing
    
    --TODO exception handling
    --handle (\e -> do putStrLn $ show e return False) $
    bracket (connectTo n (PortNumber p)) (hClose) (send)
    where    
    send hdl 
      = do
        hSetBuffering hdl NoBuffering
        f hdl 

-- no exception handling
performUnsafeSendRequest :: (Handle -> IO a) -> HostName -> PortNumber -> IO a
performUnsafeSendRequest = sendRequest

-- all IOExceptions handled return of default value
performSafeSendRequest :: (Handle -> IO a) -> a -> HostName -> PortNumber -> IO a
performSafeSendRequest f d n p
  = catch (sendRequest f n p)
          (\(e ::IOException) -> 
            do
            debugM localLogger $  show e 
            return d)

-- all IOExceptions handled return of Nothing
performMaybeSendRequest :: (Handle -> IO a) -> HostName -> PortNumber -> IO (Maybe a)
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
putMessage :: B.ByteString -> Handle -> IO ()
putMessage msg hdl
  = do
    handleAll (\e -> do
      errorM localLogger $ "putMessage: " ++ show e
      errorM localLogger $ "message: " ++ show msg 
     ) $ do
      debugM "measure.putMessage" "1"
      hPutStrLn hdl ((show $ B.length msg) ++ " ")
      B.hPut hdl msg


-- | Reads data from a stream. We define, that the first line of the message
--   is the message header which tells us how much bytes we have to read.
getMessage :: Handle -> IO (B.ByteString)
getMessage hdl
  = do
    debugM "measure.getMessage" "1"
    line <- hGetLine hdl
    let pkg = words line
    raw <- B.hGet hdl (read $ head pkg)
    return raw
  
