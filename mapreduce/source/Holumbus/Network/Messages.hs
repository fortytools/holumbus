-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.Messages
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

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
import qualified Control.Exception as E
import           Data.Binary
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

class RspMsg m where
  isError :: m -> Bool  
  getErrorMsg :: m -> String
  isUnknown :: m -> Bool
  mkErrorMsg :: String -> m



-- ----------------------------------------------------------------------------
-- Port-Handling
-- ----------------------------------------------------------------------------

data MessageException
  = TimeoutException
  | UnknownRequest
  | FalseResponse String
  | ErrorResponse String
  deriving (Show, Typeable)

    
talkWithNode 
  :: (Show a, Binary a, Show b, Binary b, RspMsg b)
  => P.Port a          -- ^ port to which the message will be send
  -> P.Stream b        -- ^ the stream from which the response is read
  -> Int               -- ^ timeout for the response in nanoseconds (1000000 = 1 sec) (0 = wait for ever)
  -> a                 -- ^ message to be send
  -> (b -> IO c)       -- ^ handler function for the response 
  -> IO c
talkWithNode p respStream timeout m hdlFct
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
        warningM localLogger "talkWithNode: timeout"
        E.throwDyn TimeoutException          
      -- handle the response
      (Just r) ->
        hdlFct r
    return res
       

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
          E.throwDyn $ ErrorResponse $ getErrorMsg rsp
        | (isUnknown rsp) = do
          warningM localLogger $ "basicResponseHandler: unknown: " ++ show rsp
          E.throwDyn UnknownRequest
        | otherwise       = do
          warningM localLogger $ "basicResponseHandler: false: " ++ show rsp
          E.throwDyn $ FalseResponse $ show rsp
        
                    
performPortAction
  :: (Show a, Binary a, Show b, Binary b, RspMsg b) 
  => P.Port a             -- ^ request port
  -> P.Stream b           -- ^ response Stream 
  -> Int                  -- ^ timeout for the response in nanoseconds (1000000 = 1 sec) (0 = wait for ever)
  -> a                    -- ^ request message
  -> (b -> IO (Maybe c))  -- ^ response handler
  -> IO c
performPortAction reqPo resStr timeout reqMsg rspHdl
  = do
    talkWithNode reqPo resStr timeout reqMsg $
      basicResponseHandler rspHdl
  
  
  
  
-- ----------------------------------------------------------------------------
-- Stream-Handling
-- ----------------------------------------------------------------------------

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
    {-
    modifyMVar mVarTid $
      \servId ->
      do
      servId' <- case servId of
        i@(Just _) -> return i
        (Nothing) ->
          do
          i <- forkIO $ requestDispatcher reqS dispatcher
          return (Just i)
      return (servId',())
-}

stopRequestDispatcher :: Thread -> IO ()
stopRequestDispatcher thread
  = do
    stopThread thread 
  {-
  modifyMVar mVarTid $
      \servId ->
      do
      servId' <- case servId of
        (Nothing) -> return Nothing
        (Just i) -> 
          do
          me <- myThreadId
          E.throwDynTo i me
          yield
          return Nothing
      return (servId',())
  -}

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
        _ <- forkIO $ dispatcher dat $ (fromJust responsePort)
        return ()

  {- = do
    E.handle (\e -> 
      do
      -- if a normal exception occurs, the dispatcher should not be killed
      errorM localLogger $ show e
      yield
      requestDispatcher reqS dispatcher
     ) $
      do
      -- catch the exception which tells us to kill the dispatcher and kill it
      E.catchDyn (requestDispatcher') (handler)
    where
    handler :: ThreadId -> IO ()
    handler i 
      = do
        debugM localLogger $ "requestDispatcher normally closed by thread " ++ show i    
    requestDispatcher'    
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
            yield
          else do
            -- do the dispatching in a new process...
            _ <- forkIO $ dispatcher dat $ (fromJust responsePort)
            return ()
        --threadDelay 10
        requestDispatcher reqS dispatcher 
-}

handleRequest
  :: (Show b, Binary b, RspMsg b)
  => P.Port b    -- ^ the reply port (where the messages will be send to)
  -> IO c        -- ^ the action which will generate the data to be send
  -> (c -> b)    -- ^ create an output from the data
  -> IO ()
handleRequest po fhdl fres
  = do
    -- in case, we can't send the error...
    E.handle (\e -> do
        errorM localLogger $ "handleRequest: exeption raised and could not be send to controller" 
        errorM localLogger $ show e) $ do
      do
      -- in case our operation fails, we send a failure-response
      E.handle (\e -> do
          errorM localLogger $ "handleRequest: exeption raised and reporting to controller" 
          errorM localLogger $ show e
          P.send po (mkErrorMsg $ show e)) $
        do
        -- our action, might raise an exception
        r <- fhdl
        -- send the response
        P.send po $ fres r