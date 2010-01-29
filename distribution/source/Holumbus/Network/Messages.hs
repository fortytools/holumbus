-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.Messages
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  
  General functions for the transmission of messages over the Holumbus-Ports.
  You don't need these functions, but they make your life much easier.
  
  The mailbox concept doesn't deal with the request and response scheme
  very well, but with this module, all the boring stuff is solved. The function
  performPortAction will do everything for you. If you find it boring to write
  a seperate listener-thread for every mailbox you want to read from, you
  might look at the startRequestDispatcher and stopRequestDispatcher functions. 

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Network.Messages
(
-- * Message-Class
  RspMsg(..)

-- * Port-Handling
, performPortAction

-- * Stream-Handling
, startRequestDispatcher
, stopRequestDispatcher
, handleRequest
)
where

import           Control.Concurrent
import		 Control.Exception	( Exception
					, throw
					)
{- 6.8
import qualified Control.Exception as E
-}

import           Data.Binary
--import           Holumbus.Common.MRBinary
import           Data.Maybe
import           Data.Typeable

import           System.Log.Logger

import           Holumbus.Common.Threading
import           Holumbus.Common.Utils
import qualified Holumbus.Network.Port as P


localLogger :: String
localLogger = "Holumbus.Network.Messages"



-- ----------------------------------------------------------------------------
-- Message-Class
-- ----------------------------------------------------------------------------

-- | The typeclass for the response messages. We want to react if an error
--   message is received an this interface helps us to detect and create such
--   a message. The unknown message will be send back, if the server doesn't
--   understand our request.
class RspMsg m where
  isError :: m -> Bool  
  getErrorMsg :: m -> String
  isUnknown :: m -> Bool
  mkErrorMsg :: String -> m



-- ----------------------------------------------------------------------------
-- Port-Handling
-- ----------------------------------------------------------------------------

-- | Every request might raise an exception
data MessageException
    = TimeoutException     	-- ^ if the server takes too long to respond
    | UnknownRequest       	-- ^ if the server doesn't understand our request
    | FalseResponse String 	-- ^ when the server response doesn't match our definition
    | ErrorResponse String	-- ^ if an error in the server occurred and he informs us about the error
      deriving (Show, Typeable)

instance Exception MessageException

-- | Sends a repest to the server  (stream) and waits for a response.
--   If the response can't be received in a certain time, a TimeoutException
--   will be raised. If a response is received, an individual response handler
--   is executed.
talkWithServer
  :: (Show a, Binary a, Show b, Binary b, RspMsg b)
  => P.Port a          -- ^ port to which the message will be send
  -> P.Stream b        -- ^ the stream from which the response is read
  -> Int               -- ^ timeout for the response in nanoseconds (1000000 = 1 sec) (0 = wait for ever)
  -> a                 -- ^ message to be send
  -> (b -> IO c)       -- ^ handler function for the response 
  -> IO c
talkWithServer p respStream timeout m hdlFct
  = do
    respPort <- P.newPortFromStream respStream
    -- send the request to the node
    debugM localLogger $ "sending: " ++ show m
    P.sendWithGeneric p m (encode respPort)
    --wait for the response
    debugM localLogger $ "waiting for response for: " ++ show m 
    response <- P.tryWaitReadStream respStream timeout
    -- r' <- P.readStream respStream
    -- let response = Just r'
    debugM localLogger "response Message..."
    debugM localLogger $ show response
    res <- case response of
      -- if no response
      Nothing -> do
        warningM localLogger "talkWithServer: timeout"
        {- E.throwDyn TimeoutException -}
	throw TimeoutException
      -- handle the response
      (Just r) ->
        hdlFct r
    return res
       

-- | A wrapper around the user defined response handler.
--   All error and unkown response will be catched, so you don't have to
--   deal with them. But you can't also throw an error in your response 
--   function, if you want.
basicResponseHandler
  :: (Show b, Binary b, RspMsg b) 
  => (b -> IO (Maybe c)) 
  -> b 
  -> IO c
basicResponseHandler hdlFct rsp
  = do
    -- look for right message
    res <- hdlFct rsp
    case res of
      -- if right type... return result
      (Just r) -> 
        return r
      -- else handle error types
      Nothing ->
        handleError
    where
      handleError
        | (isError rsp)   = do
			    warningM localLogger $ "basicResponseHandler: error: " ++ show rsp
			    {- 6.8 E.throwDyn $ ErrorResponse $ getErrorMsg rsp -}
			    throw $ ErrorResponse $ getErrorMsg rsp
        | (isUnknown rsp) = do
			    warningM localLogger $ "basicResponseHandler: unknown: " ++ show rsp
			    {- 6.8 E.throwDyn UnknownRequest -}
			    throw UnknownRequest
        | otherwise       = do
			    warningM localLogger $ "basicResponseHandler: false: " ++ show rsp
			    {- E.throwDyn $ FalseResponse $ show rsp -}
			    throw $ FalseResponse $ show rsp
        
                    
-- | Sends a request to the server (stream) and handles the response and all
--   error cases. Very helpful when simulating a request response scheme
--   with the mailboxes.                    
performPortAction
  :: (Show a, Binary a, Show b, Binary b, RspMsg b) 
  => P.Port a             -- ^ request port
  -> P.Stream b           -- ^ response Stream 
  -> Int                  -- ^ timeout for the response in mikroseconds (1000000 = 1 sec) (0 = wait for ever)
  -> a                    -- ^ request message
  -> (b -> IO (Maybe c))  -- ^ response handler
  -> IO c
performPortAction reqPo resStr timeout reqMsg rspHdl
  = do
    talkWithServer reqPo resStr timeout reqMsg $
      basicResponseHandler rspHdl
  

-- ----------------------------------------------------------------------------
-- Stream-Handling
-- ----------------------------------------------------------------------------

-- | The server-side request dispatcher handles all incomming responses.
--   The dispatcher runs in its own thread and should not be killed by
--   any exceptions which will be raised in the handling process.
startRequestDispatcher
  :: (Binary a, Show a, Show b, Binary b, RspMsg b) 
  => Thread                                     -- ^ threadId, to be filled
  -> P.Stream a                                 -- ^ request-Stream (this is where the messages come in)
  -> (a -> P.Port b -> IO ())                   -- ^ the dispatcher (create a reply message)
  -> IO ()
startRequestDispatcher thread reqS dispatcher
  = do
    setThreadAction (requestDispatcher reqS dispatcher) thread
    startThread thread


-- | Stops the request dispatcher.
stopRequestDispatcher :: Thread -> IO ()
stopRequestDispatcher thread
  = do
    stopThread thread 


-- | Wrapper around the user-defined dispatching function. For every incomming
--   request a new thread will be created to be able to handle the next request. 
requestDispatcher 
  :: (Binary a, Show a, Show b, Binary b, RspMsg b)
  => P.Stream a 
  -> (a -> P.Port b -> IO ())
  -> IO ()
requestDispatcher reqS dispatcher
  = do
    -- read the next message from the stream (block, if no message arrived)
    msg <- P.readStreamMsg reqS
    -- extract the data
    let dat = P.getMessageData msg
    debugM localLogger "dispatching new Message... "
    debugM localLogger $ show dat
    -- extract the (possible replyport)
    let responsePort = decodeMaybe $ P.getGenericData msg
    if (isNothing responsePort)
      then do
        warningM localLogger "no reply port in message"
        -- yield
      else do
        -- do the dispatching in a new process...
        infoM localLogger $ "forking dispatch with response port: "  ++ (show responsePort)
        _ <- forkIO $ dispatcher dat $ (fromJust responsePort)
        return ()


-- | Execute a function and send its result to the specified port.
handleRequest
  :: (Show b, Binary b, RspMsg b)
  => P.Port b    -- ^ the reply port (where the messages will be send to)
  -> IO c        -- ^ the action which will generate the data to be send
  -> (c -> b)    -- ^ create an output from the data
  -> IO ()
handleRequest po fhdl fres
  = do
    -- in case, we can't send the error...
    handleAll (\e -> do
        errorM localLogger $ "handleRequest: exeption raised and could not be send to controller" 
        errorM localLogger $ show e) $ do
      do
      -- in case our operation fails, we send a failure-response
      handleAll ( \e -> do
		        errorM localLogger $ "handleRequest: exeption raised and reporting to controller" 
		        errorM localLogger $ show e
                        P.send po (mkErrorMsg $ show e)
		) $
                do
		-- our action, might raise an exception
		r <- fhdl
		-- send the response
		P.send po $ fres r
