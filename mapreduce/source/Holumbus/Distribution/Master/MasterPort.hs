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


instance Master MasterPort where


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


  startJob ji c@(MasterPort p)
    = do
      withStream $
        \s -> performPortAction p s (MReqStartJob ji) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just c)
            _ -> return Nothing


  printDebug (MasterPort p)
    = do
      putStrLn "MasterPort:"
      putStrLn $ show p