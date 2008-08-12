-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Master.MasterPort
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Master.MasterPort
(
-- * Datatypes
  MasterPort
  
-- * Creation and Destruction
, newMasterPort
)
where

import Holumbus.Network.Port
import Holumbus.Network.Site

import Holumbus.Common.Debug
import Holumbus.MapReduce.MapReduce
import Holumbus.Distribution.Messages
import Holumbus.Distribution.Master


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data MasterPort = MasterPort MasterRequestPort
  deriving (Show)



-- ----------------------------------------------------------------------------
-- Creation and Destruction
-- ----------------------------------------------------------------------------


-- | Creates a new MasterPort
newMasterPort :: MasterRequestPort -> MasterPort
newMasterPort p = MasterPort p



-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------

instance Debug MasterPort where

  printDebug (MasterPort p)
    = do
      putStrLn "MasterPort:"
      putStrLn $ show p
      
      
     

instance MapReduce MasterPort where


  closeMapReduce md
    = closeMaster md


  getMySiteId _
    = getSiteId

  
  getMapReduceType _
    = return MRTMaster

  
  startControlling (MasterPort p)
    = withStream $
       \s -> performPortAction p s (MReqStartControlling) $
         \rsp ->
         do
         case rsp of
           (MRspSuccess) -> return (Just ())
           _ -> return Nothing

  
  stopControlling (MasterPort p)
    = withStream $
       \s -> performPortAction p s (MReqStopControlling) $
         \rsp ->
         do
         case rsp of
           (MRspSuccess) -> return (Just ())
           _ -> return Nothing


  isControlling (MasterPort p)
    = withStream $
       \s -> performPortAction p s (MReqIsControlling) $
         \rsp ->
         do
         case rsp of
           (MRspIsControlling b) -> return (Just b)
           _ -> return Nothing
  

  
  doSingleStep (MasterPort p)
    = withStream $
        \s -> performPortAction p s (MReqSingleStep) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just ())
            _ -> return Nothing

  
  doMapReduce ji (MasterPort p)
    = withStream $
        \s -> performPortAction p s (MReqPerformJob ji) $
          \rsp ->
          do
          case rsp of
            (MRspResult r) -> return (Just r)
            _ -> return Nothing




instance Master MasterPort where


  closeMaster _ = return ()


  getMasterRequestPort (MasterPort p) = p
  
  
  registerWorker sid po c@(MasterPort p) 
    = do
      withStream $
        \s -> performPortAction p s (MReqRegister sid po) $
          \rsp ->
          do
          case rsp of
            (MRspRegister n) -> return (Just (n,c))
            _ -> return Nothing


  unregisterWorker wid c@(MasterPort p)
    = do
      withStream $
        \s -> performPortAction p s (MReqUnregister wid) $
          \rsp ->
          do
          case rsp of
            (MRspUnregister) -> return (Just c)
            _ -> return Nothing


  receiveTaskCompleted td c@(MasterPort p)
    = do
      withStream $
        \s -> performPortAction p s (MReqTaskCompleted td) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just c)
            _ -> return Nothing
    

  receiveTaskError td c@(MasterPort p)
    = do
      withStream $
        \s -> performPortAction p s (MReqTaskError td) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just c)
            _ -> return Nothing