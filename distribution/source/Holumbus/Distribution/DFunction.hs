-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DFunction
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XFlexibleContexts #-}
module Holumbus.Distribution.DFunction 
(
    DFunction
  , BinaryFunction
    
  , newDFunction
  , newRemoteDFunction
  , closeDFunction
  
  , accessDFunction
)
where

import           Prelude hiding (catch)

import           Control.Exception
import           Data.Binary
--import           Holumbus.Common.MRBinary
import qualified Data.ByteString.Lazy as B
import           System.IO
import           System.Log.Logger

import           Holumbus.Distribution.DNode.Base


localLogger :: String
localLogger = "Holumbus.Distribution.DFunction"

-- ----------------------------------------------------------------------------
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

dFunctionType :: DRessourceType
dFunctionType = mkDRessourceType "DFUNCTION"

mkDFunctionEntry :: (BinaryFunction a) => DFunctionReference a -> DRessourceEntry
mkDFunctionEntry d = DRessourceEntry {
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


data DFunction a
  = DFunctionLocal DRessourceAddress a
  | DFunctionRemote DRessourceAddress

instance Binary (DFunction a) where
  put(DFunctionLocal dra _) = put dra
  put(DFunctionRemote dra)  = put dra
  get = get >>= \dra -> return (DFunctionRemote dra)
  
  
data DFunctionReference a = DFunctionReference DRessourceAddress a



newDFunction :: (BinaryFunction a) => String -> a -> IO (DFunction a)
newDFunction s f
  = do
    a <- genLocalRessourceAddress dFunctionType s
    let df  = (DFunctionLocal a f)
        dfr = (DFunctionReference a f)
        dfd = (mkDFunctionEntry dfr)
    addLocalRessource a dfd
    return df


newRemoteDFunction :: (BinaryFunction a) => String -> String -> IO (DFunction a)
newRemoteDFunction r n
  = do
    return $ DFunctionRemote dra
    where
    dra = mkDRessourceAddress dFunctionType r n


closeDFunction :: DFunction a -> IO ()
closeDFunction (DFunctionLocal dra _)
  = do
    delLocalRessource dra
closeDFunction (DFunctionRemote dra)
  = do
    delForeignRessource dra


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


accessDFunction :: (BinaryFunction a) => DFunction a -> a
accessDFunction (DFunctionLocal _ f) = f
accessDFunction (DFunctionRemote a)
  = remoteCall $ \bs ->
      unsafeAccessForeignRessource a (requestCall bs)


{-
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
