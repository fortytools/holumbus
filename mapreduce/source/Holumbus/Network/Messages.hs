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
  RspMsg(..)
, performPortAction
)
where

import qualified Control.Exception as E
import           Data.Binary
import           Data.Typeable

import           System.Log.Logger

import qualified Holumbus.Network.Port as P


localLogger :: String
localLogger = "Holumbus.Network.Messages"



-- ----------------------------------------------------------------------------
-- request an response handling
-- ----------------------------------------------------------------------------

class RspMsg m where
  isError :: m -> Bool  
  getErrorMsg :: m -> String  
  isUnknown :: m -> Bool

    
data MessageException
  = TimeoutException
  | UnknownRequest
  | FalseResponse String
  | ErrorResponse String
  deriving (Show, Typeable)

    
talkWithNode 
  :: (Show a, Binary a, Show b, Binary b, RspMsg b) => P.Port a    -- ^ port to which the message will be send
  -> P.Stream b 
  -> a                                 -- ^ message to be send
  -> (b -> IO c)       -- ^ handler function for the response 
  -> IO c
talkWithNode p respStream m hdlFct
  = do
    -- create the response stream and the response port
    --respStream <- (P.newStream::IO NodeResponseStream)
    respPort <- P.newPortFromStream respStream
    -- send the request to the node
    -- putStrLn $ show respPort
    debugM localLogger $ "sending: " ++ show m
    P.sendWithGeneric p m (encode respPort)
    --wait for the response
    debugM localLogger $ "waiting for response for: " ++ show m 
    -- response <- P.tryWaitReadStream respStream P.time30
    r' <- P.readStream respStream
    let response = Just r'
    debugM localLogger "response Message..."
    debugM localLogger $ show response
    res <- case response of
      -- if no response
      Nothing ->
        E.throwDyn TimeoutException          
      -- handle the response
      (Just r) ->
        hdlFct r
    -- close the stream
    --P.closeStream respStream
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
        | (isError rsp)   = E.throwDyn $ ErrorResponse $ getErrorMsg rsp
        | (isUnknown rsp) = E.throwDyn UnknownRequest
        | otherwise       = E.throwDyn $ FalseResponse $ show rsp
        
                    
performPortAction
  :: (Show a, Binary a, Show b, Binary b, RspMsg b) 
  => P.Port a             -- ^ request port
  -> P.Stream b           -- ^ response Stream 
  -> a                    -- ^ request message
  -> (b -> IO (Maybe c))  -- ^ response handler
  -> IO c
performPortAction reqPo resStr reqMsg rspHdl
  = do
    talkWithNode reqPo resStr reqMsg $
      basicResponseHandler rspHdl
  
