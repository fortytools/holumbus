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

module Examples.Distribution.ChatDChan.MessagesDChan
(
    ChatRequest(..)
  , ChatResponse(..)
  
  , ClientToken
  , genClientToken
)
where

import           Data.Binary
--import           Holumbus.Common.MRBinary
import           System.Random
import           Holumbus.Distribution.DChan


data ChatRequest
  = ReqRegister String (DChan ChatResponse)
  | ReqUnregister ClientToken
  | ReqMessage ClientToken String

instance Binary ChatRequest where
  put(ReqRegister cn ch)  = putWord8 1 >> put cn >> put ch
  put(ReqUnregister ct)   = putWord8 2 >> put ct
  put(ReqMessage ct text) = putWord8 3 >> put ct >> put text
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \cn -> get >>= \ch -> return (ReqRegister cn ch)
        2 -> get >>= \ct -> return (ReqUnregister ct)
        3 -> get >>= \ct -> get >>= \text -> return (ReqMessage ct text)
        _ -> error "ChatRequest: wrong encoding"

data ChatResponse
  = RspRegister ClientToken
  | RspMessage String String
  | RspError String
  
instance Binary ChatResponse where
  put(RspRegister ct)     = putWord8 1 >> put ct
  put(RspMessage cn text) = putWord8 2 >> put cn >> put text
  put(RspError e)         = putWord8 3 >> put e
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \ct -> return (RspRegister ct)
        2 -> get >>= \cn -> get >>= \text -> return (RspMessage cn text)
        3 -> get >>= \e -> return (RspError e)
        _ -> error "ChatResponse: wrong encoding"


data ClientToken = ClientToken Integer Integer
  deriving (Show, Eq, Ord)

instance Binary ClientToken where
  put(ClientToken i rnd) = put i >> put rnd
  get = get >>= \i -> get >>= \rnd -> return (ClientToken i rnd)

genClientToken :: Integer -> IO ClientToken
genClientToken i
  = do
    rnd <- getStdRandom (randomR (1,1000000000))
    return $ ClientToken i rnd
  
