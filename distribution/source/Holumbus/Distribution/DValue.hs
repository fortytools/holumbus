-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DValue
  Copyright  : Copyright (C) 2010 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module offers the distrubted value datatype.
  
  A DValue is like a distributed MVar, but is can only be
  set once on the local node. If the value is set, it cannot be
  changed any more.

  I don't know if this is useful at all, so the implementation is not
  quite finished an some things could be improved.
-}

-- ----------------------------------------------------------------------------

module Holumbus.Distribution.DValue
(
  -- * datatypes
    DValue
    
  -- * creating and closing a DValue
  , newDValue
  , newRemoteDValue
  , closeDValue

  -- * access a DValue
  , getDValue
  -- , flushDValue
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


-- | Creates new DValue on the local DNode. The first parameter
--   is the name of the value which could be used in other processes to
--   access this stream. If you leave it empty, a random Id will be created.
newDValue :: (Binary a) => String -> a -> IO (DValue a)
newDValue s a
  = do
    dv <- newDMVar s a
    return (DValueLocal dv)
    

-- | Creates a new DValue.
--   The first parameter is the name of the resource and the second one
--   the name of the node.
newRemoteDValue :: String -> String -> IO (DValue a)
newRemoteDValue r n
  = do
    dv <- newRemoteDMVar r n
    lv <- newEmptyMVar
    return (DValueRemote dv lv)


-- | Closes a DValue.
closeDValue :: (DValue a) -> IO ()
closeDValue (DValueLocal dv)
  = do
    closeDMVar dv
closeDValue (DValueRemote dv _)
  = do
    closeDMVar dv


-- | Gets value. It will only access the network on the first
--   time and my throw an exception. It returns always (Just a)
--    but this may change in the future, so the type sticks to (Maybe a)
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


{-
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
-}

