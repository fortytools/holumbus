-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DFunction
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module offers distributed functions.

  This idea behind this is to implement RPC based on DNodes. You specify
  a function which could be called from other programs and register this
  as a resource in your local DNode. Then the foreign DNodes can create
  a link to this function an execute it. The function parameters will be
  serialized and send to the local DNode. There the parameters are deserialized
  and the function will be called. After this the return-value will be send
  back to the calling node. 
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XFlexibleContexts #-}
module Holumbus.Distribution.DFunction 
(
  -- * datatypes
    DFunction
  , BinaryFunction
   
  -- * creating and closing function references
  , newDFunction
  , newRemoteDFunction
  , closeDFunction
  
  -- * invoking functions
  , accessDFunction
)
where

import           Prelude hiding (catch)

import           Control.Exception
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           System.IO
import           System.Log.Logger

import           Holumbus.Distribution.DNode.Base


localLogger :: String
localLogger = "Holumbus.Distribution.DFunction"

-- ----------------------------------------------------------------------------

-- | Binary function typeclass. You can only use functions whose parameters
--   and return value are serializable. The idea of this typeclass comes from
--   the haxr library by Bjorn Bringert (http://www.haskell.org/haskellwiki/HaXR)
class BinaryFunction a where
    toFun :: a -> [B.ByteString] -> IO B.ByteString
    remoteCall :: ([B.ByteString] -> IO B.ByteString) -> a

instance (Binary a) => BinaryFunction (IO a) where
    toFun x [] = x >>= return . encode
    toFun _ _ = fail "Too many arguments"
    remoteCall f = f [] >>= return . decode

instance (Binary a, BinaryFunction b) => BinaryFunction (a -> b) where
    toFun f (x:xs) = toFun (f (decode x)) xs
    toFun _ _ = error "Too few arguments"
    remoteCall f x = remoteCall (\xs -> f (encode x:xs))




-- ----------------------------------------------------------------------------

dFunctionType :: DResourceType
dFunctionType = mkDResourceType "DFUNCTION"

mkDFunctionEntry :: (BinaryFunction a) => DFunctionReference a -> DResourceEntry
mkDFunctionEntry d = DResourceEntry {
    dre_Dispatcher   = dispatchDFunctionRequest d 
  }


data DFunctionRequestMessage
  = DFMReqCall [B.ByteString]
  deriving (Show)
  
instance Binary DFunctionRequestMessage where
  put(DFMReqCall bs) = put bs
  get = get >>= \bs -> return (DFMReqCall bs)


data DFunctionResponseMessage
  = DFMRspCallResult B.ByteString
  | DFMRspCallException String

instance Binary DFunctionResponseMessage where
  put(DFMRspCallResult b) = putWord8 1 >> put b
  put(DFMRspCallException e) = putWord8 2 >> put e
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \b -> return (DFMRspCallResult b)
        2 -> get >>= \e -> return (DFMRspCallException e)
        _ -> error "DFunctionResponseMessage: wrong encoding"


dispatchDFunctionRequest :: (BinaryFunction a) => DFunctionReference a -> DNodeId -> Handle -> IO () 
dispatchDFunctionRequest dfun _ hdl
  = do
    debugM localLogger "dispatcher: getting message from handle"
    raw <- getByteStringMessage hdl
    let msg = (decode raw)::(DFunctionRequestMessage)
    -- debugM localLogger $ "dispatcher: Message: " ++ show msg
    case msg of
      (DFMReqCall l)  -> handleCall dfun l hdl


-- | The DFunction datatype. This is more like a reference to
--   a function located on a different node. You can call this
--   function via the accessDFunction function.
data DFunction a
  = DFunctionLocal DResourceAddress a
  | DFunctionRemote DResourceAddress

instance Binary (DFunction a) where
  put(DFunctionLocal dra _) = put dra
  put(DFunctionRemote dra)  = put dra
  get = get >>= \dra -> return (DFunctionRemote dra)
  
  
data DFunctionReference a = DFunctionReference DResourceAddress a


-- | Creates a new distributed function. Only functions which are registered
--   at the local node can be called from the outside. The string parameter
--   specifies the name of the function which could the used by other nodes
--   to call it. If you leave it empty, a random name will be generated.
newDFunction :: (BinaryFunction a) => String -> a -> IO (DFunction a)
newDFunction s f
  = do
    a <- genLocalResourceAddress dFunctionType s
    let df  = (DFunctionLocal a f)
        dfr = (DFunctionReference a f)
        dfd = (mkDFunctionEntry dfr)
    addLocalResource a dfd
    return df


-- | Created a reference to a function on a remote node. The first parameter
--   is the name of the function, the second parameter is the name of the node.
newRemoteDFunction :: (BinaryFunction a) => String -> String -> IO (DFunction a)
newRemoteDFunction r n
  = do
    return $ DFunctionRemote dra
    where
    dra = mkDResourceAddress dFunctionType r n


-- | Closes a DFunction reference.
closeDFunction :: DFunction a -> IO ()
closeDFunction (DFunctionLocal dra _)
  = do
    delLocalResource dra
closeDFunction (DFunctionRemote dra)
  = do
    delForeignResource dra


requestCall :: [B.ByteString] -> Handle -> IO B.ByteString
requestCall bs hdl
  = do
    putByteStringMessage (encode $ DFMReqCall bs) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DFMRspCallResult b) -> return b
      (DFMRspCallException e) -> throwIO $ DistributedException e "requestCall" "DFunction"


handleCall :: BinaryFunction a => DFunctionReference a -> [B.ByteString] -> Handle -> IO ()
handleCall (DFunctionReference _ f) bs hdl
  = do
    catch
      (do
       b <- toFun f bs
       putByteStringMessage (encode $ DFMRspCallResult b) hdl)
      (\(SomeException e) -> do
       putByteStringMessage (encode $ DFMRspCallException (show e)) hdl)


-- | Transforms a DFunction object to a normal function which could be called and passed around.
--   Because you have network tranfer everytime you call the function, this might throw a
--   DistributedException when the foreign node becomes unreachable.
accessDFunction :: (BinaryFunction a) => DFunction a -> a
accessDFunction (DFunctionLocal _ f) = f
accessDFunction (DFunctionRemote a)
  = remoteCall $ \bs ->
      unsafeAccessForeignResource a (requestCall bs)


{-
Example Code:

addInt :: Int -> Int -> IO Int
addInt i1 i2 = return $ i1 + i2 

test :: IO ()
test
  = do
    -- dfun <- newDFunction "add" (addInt)
    dfun <- (newRemoteDFunction address)::(IO (DFunction (Int -> Int -> IO Int)))
    let f = accessDFunction dfun
    res <- f 1 2
    putStrLn $ show res
    return ()
-}
