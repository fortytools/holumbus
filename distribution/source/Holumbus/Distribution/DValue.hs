-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DValue
  Copyright  : Copyright (C) 2010 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Distribution.DValue
(
    DValue
    
  , newDValue
  , newRemoteDValue
  , closeDValue

  , getDValue
  , flushDValue
)
where

import           Control.Concurrent.MVar

import           Data.Binary

import           Holumbus.Distribution.DMVar


-- localLogger :: String
--localLogger = "Holumbus.Distribution.DValue"


data DValue a
  = DValueLocal (DMVar a)
  | DValueRemote (DMVar a) (MVar a)


newDValue :: (Binary a) => String -> a -> IO (DValue a)
newDValue s a
  = do
    dv <- newDMVar s a
    return (DValueLocal dv)
    

newRemoteDValue :: String -> String -> IO (DValue a)
newRemoteDValue r n
  = do
    dv <- newRemoteDMVar r n
    lv <- newEmptyMVar
    return (DValueRemote dv lv)


closeDValue :: (DValue a) -> IO ()
closeDValue (DValueLocal dv)
  = do
    closeDMVar dv
closeDValue (DValueRemote dv _)
  = do
    closeDMVar dv


getDValue :: (Binary a) => DValue a -> IO (Maybe a)
getDValue (DValueLocal dv)
  = do
    a <- readDMVar dv
    return (Just a)
getDValue (DValueRemote dv lv)
  = do
    e <- isEmptyMVar lv
    if e
      then do
        --TODO exception handling here...
        a <- readDMVar dv
        putMVar lv a
        return (Just a)
      else do
        a <- readMVar lv
        return (Just a)


flushDValue :: DValue a -> IO ()
flushDValue (DValueLocal _) = return ()
flushDValue (DValueRemote _ lv)
  = do
    e <- isEmptyMVar lv
    if e
      then return ()
      else do
        _ <- takeMVar lv
        return ()

