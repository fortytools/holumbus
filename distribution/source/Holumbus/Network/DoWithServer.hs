-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.DoWithServer
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides an interface to start a server listeing on a tcp socket.
  ( Module is based on simple tcp server found here http://sequence.complete.org/node/258 )
  
  You have to implement 2 functions
       type ServerAction  a = a -> Client a -> [Client a] -> IO [Client a]
    
    and
    
      type LineConverter a = String -> a
      
   to start a server use the
     doWithServer :: (Show a) => Int -> ServerAction a -> LineConverter a -> String -> IO ()
   function.

-}

-- ----------------------------------------------------------------------------
module Holumbus.Network.DoWithServer
(
    Client (..)
  , ServerAction
  , LineConverter
  , doWithServer
)
 where 

--import           Holumbus.Common.Logging
import System.Log.Logger
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (liftM)
import Network
import Holumbus.Common.Utils  ( handleAll )

localLogger :: String
localLogger = "Holumbus.Network.DoWithServer"

------------------------------------------------------------------------------------------------------
-- Datatypes

-- | the connecting client type
data Client        a = Client (TChan a) Handle HostName PortNumber

instance Show (Client a) where
  show (Client _ _ host port) = "Client " ++ host ++ ":"++show port
  
instance Eq (Client a) where
  (==) (Client _ _ host1 port1) (Client _ _ host2 port2) = host1 == host2 && port1 == port2
  (/=) c1 c2 = not (c1 == c2)
  
instance Ord (Client a) where
  compare (Client _ _ host1 port1) (Client _ _ host2 port2) = compare (host1,port1) (host2,port2)

-- | type for call back function that is executed within server cycle
type ServerAction  a = a -> Client a -> [Client a] -> IO [Client a]

-- | converts the string read by hGetLine to your datatype
type LineConverter a = String -> a
------------------------------------------------------------------------------------------------------

-- | execute a ServerAction wrapped by the tcp server
doWithServer :: (Show a) => Int -> ServerAction a -> LineConverter a -> String -> IO ()
doWithServer port action converter prompt = 
  withSocketsDo $ do
    servSock <- listenOn . PortNumber . fromIntegral $ port
    handleAll (\_ -> sClose servSock) $ do
      acceptChan <- atomically newTChan
      forkIO $ acceptLoop servSock acceptChan converter prompt
      mainLoop servSock acceptChan [] action prompt --`finally` sClose servSock


-- | the accept loop for new connections
acceptLoop :: Socket -> TChan (Client a) -> LineConverter a -> String -> IO ()
acceptLoop servSock chan convert prompt = do
  (handle, host, port) <- accept servSock
  hPrompt handle prompt
  ch <- atomically newTChan
  forkIO $ clientLoop (Client ch handle host port) convert
  atomically $ writeTChan chan (Client ch handle host port)
  acceptLoop servSock chan convert prompt

-- | the listenerloops for getting the clients commands
clientLoop :: Client a -> LineConverter a -> IO ()
clientLoop client@(Client chan handle _ _) convert =
  handleAll (\_ -> do { infoM localLogger $ "Client disconnected: "++show client; hClose handle; return () }) $ do
    listenLoop (liftM convert $ hGetLine handle) chan

-- | the listenerloops for getting the clients commands
listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan =
  sequence_ (repeat (act >>= atomically . writeTChan chan)) 

-- | the main loop combines new client actions (Left) and new command actions (Right)
mainLoop :: (Show a) => Socket -> TChan (Client a) -> [(Client a)] -> ServerAction a -> String-> IO ()
mainLoop servSock acceptChan clients f prompt= do
  r <- atomically $ (Left `fmap` readTChan acceptChan)
                    `orElse`
                    (Right `fmap` tselect clients)
  case r of
    -- a new client
    Left sender -> do
      infoM localLogger $ "new client" ++ show sender
      mainLoop servSock acceptChan (sender:clients) f prompt
   
    -- a new command
    Right (line, sender) -> do
      debugM localLogger $ (show sender) ++ " [ " ++ show line ++ " ]"
      clients' <- f line sender clients
      debugM localLogger $ "Num of Clients: " ++ show (length clients')
      mainLoop servSock acceptChan (clients') f prompt


hPrompt :: Handle -> String -> IO ()
hPrompt h p = do
  openhandle <- hIsOpen h
  if openhandle then do
    hPutStr h p
    hFlush h
    else return ()

tselect :: [(Client a)] -> STM (a, Client a)
tselect = foldl orElse retry . map (\client@(Client ch _ _ _) -> (\line -> (line,client)) `fmap` readTChan ch) 