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

module MessagesDChan
(
    RegisterClientFunction
  , UnregisterClientFunction
  , SendChatMessageFunction
  , mkRegisterClientFunction
  , mkUnregisterClientFunction
  , mkSendChatMessageFunction

  , ReceiveChatMessageFunction
)
where

import           Holumbus.Distribution.DFunction

-- Interface of the chat server
type RegisterClientFunction = String -> DFunction ReceiveChatMessageFunction -> IO (Maybe Int)
type UnregisterClientFunction = Int -> IO ()
type SendChatMessageFunction = Int -> String -> IO ()

-- interface of the client
type ReceiveChatMessageFunction = String -> String -> IO ()


mkRegisterClientFunction :: String -> IO RegisterClientFunction
mkRegisterClientFunction = mkNamedServerFunction "register"

mkUnregisterClientFunction :: String -> IO UnregisterClientFunction
mkUnregisterClientFunction = mkNamedServerFunction "unregister"

mkSendChatMessageFunction :: String -> IO SendChatMessageFunction
mkSendChatMessageFunction = mkNamedServerFunction "send"

mkNamedServerFunction :: (BinaryFunction a) => String -> String -> IO a
mkNamedServerFunction fn sn
  = do
    df <- newRemoteDFunction fn sn
    return (accessDFunction df)

