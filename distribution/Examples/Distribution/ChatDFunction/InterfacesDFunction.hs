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

module InterfacesDFunction
(
    RegisterClientFunction
  , UnregisterClientFunction
  , SendChatMessageFunction

  , ServerInterfaceStub
  , RemoteServerInterface(..)
  , createLocalServerInterfaceStub
  , closeLocalServerInterfaceStub
  , createRemoveServerInterface



  , ReceiveChatMessageFunction

  , ClientInterfaceStub
  , RemoteClientInterface(..)
  , createLocalClientInterfaceStub
  , closeLocalClientInterfaceStub
  , createRemoteClientInterface
)
where

import           Data.Binary

import           Holumbus.Distribution.DFunction


-- ----------------------------------------------------------------------------
-- the servers' interface
-- ----------------------------------------------------------------------------

-- functions of the chat server
type RegisterClientFunction = String -> ClientInterfaceStub -> IO (Maybe Int)
type UnregisterClientFunction = Int -> IO ()
type SendChatMessageFunction = Int -> String -> IO ()

-- in theory this could be made binary, but we don't need this
data ServerInterfaceStub = ServerInterfaceStub {
    sifs_register   :: DFunction RegisterClientFunction
  , sifs_unregister :: DFunction UnregisterClientFunction
  , sifs_send       :: DFunction SendChatMessageFunction
  }

-- this cannot be binary, but it is much nicer to call the real functions
data RemoteServerInterface = RemoteServerInterface {
    sif_register   :: RegisterClientFunction
  , sif_unregister :: UnregisterClientFunction
  , sif_send       :: SendChatMessageFunction
  }

createLocalServerInterfaceStub
  :: RegisterClientFunction
  -> UnregisterClientFunction
  -> SendChatMessageFunction
  -> IO ServerInterfaceStub
createLocalServerInterfaceStub f1 f2 f3
  = do
    df1 <- newDFunction "register" f1
    df2 <- newDFunction "unregister" f2
    df3 <- newDFunction "send" f3
    return (ServerInterfaceStub df1 df2 df3)

closeLocalServerInterfaceStub :: ServerInterfaceStub -> IO ()
closeLocalServerInterfaceStub (ServerInterfaceStub df1 df2 df3)
  = do
    closeDFunction df1
    closeDFunction df2
    closeDFunction df3

createRemoveServerInterface :: String -> IO RemoteServerInterface
createRemoveServerInterface sn
  = do
    f1 <- mkNamedServerFunction "register" sn
    f2 <- mkNamedServerFunction "unregister" sn
    f3 <- mkNamedServerFunction "send" sn
    return (RemoteServerInterface f1 f2 f3)
    where
    mkNamedServerFunction :: (BinaryFunction a) => String -> String -> IO a
    mkNamedServerFunction fn sn
      = do
        df <- newRemoteDFunction fn sn
        return (accessDFunction df)


-- ----------------------------------------------------------------------------
-- the client's interface
-- ----------------------------------------------------------------------------

-- functions of the client
type ReceiveChatMessageFunction = String -> String -> IO ()

-- this needs to be binary, because we send this to the server
data ClientInterfaceStub = ClientInterfaceStub {
    cifs_receive :: DFunction ReceiveChatMessageFunction
  }

instance Binary ClientInterfaceStub where
  put (ClientInterfaceStub f1) = put f1
  get = get >>= \f1 -> return (ClientInterfaceStub f1)

-- this cannot be binary, but it is much nicer to call the real functions
data RemoteClientInterface = RemoteClientInterface {
    cif_receive :: ReceiveChatMessageFunction
  }

createLocalClientInterfaceStub :: ReceiveChatMessageFunction -> IO ClientInterfaceStub
createLocalClientInterfaceStub f1
  = do
    df1 <- newDFunction "receive" f1
    return (ClientInterfaceStub df1)

closeLocalClientInterfaceStub :: ClientInterfaceStub -> IO ()
closeLocalClientInterfaceStub (ClientInterfaceStub df1)
  = do
    closeDFunction df1

createRemoteClientInterface :: ClientInterfaceStub -> RemoteClientInterface
createRemoteClientInterface (ClientInterfaceStub df1)
  = (RemoteClientInterface f1)
    where
    f1 = (accessDFunction df1)

