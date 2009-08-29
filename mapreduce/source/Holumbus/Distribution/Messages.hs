-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Messages
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Messages
(
-- * Datatypes
  WorkerId

-- * Messages to and from the Master
, MasterRequestMessage(..)
, MasterResponseMessage(..)

-- * Messages to and from the Worker
, WorkerRequestMessage(..)
, WorkerResponseMessage(..)

-- * request an response handling
, performPortAction -- reexport from Holumbus.Network.Messages
)
where


import           Data.Binary
--import           Holumbus.Common.MRBinary

import           Holumbus.Network.Messages
import           Holumbus.MapReduce.Types


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

type WorkerId = Int



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


-- Messages to and from the Master
data MasterRequestMessage 
  = MReqTaskCompleted TaskData
  | MReqTaskError TaskData  
  | MReqStartControlling
  | MReqStopControlling
  | MReqIsControlling
  | MReqSingleStep
  | MReqPerformJob JobInfo
  | MReqUnknown
  deriving (Show)


instance Binary MasterRequestMessage where
  put (MReqTaskCompleted td) = putWord8 1 >> put td
  put (MReqTaskError td)     = putWord8 2 >> put td  
  put (MReqStartControlling) = putWord8 3
  put (MReqStopControlling)  = putWord8 4
  put (MReqIsControlling)    = putWord8 5
  put (MReqSingleStep)       = putWord8 6
  put (MReqPerformJob ji)    = putWord8 7 >> put ji
  put (MReqUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \td -> return (MReqTaskCompleted td)
        2 -> get >>= \td -> return (MReqTaskError td) 
        3 -> return (MReqStartControlling)
        4 -> return (MReqStopControlling)
        5 -> return (MReqIsControlling)
        6 -> return (MReqSingleStep)
        7 -> get >>= \ji -> return (MReqPerformJob ji)
        _ -> return (MReqUnknown)



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


data MasterResponseMessage 
  = MRspSuccess
  | MRspIsControlling Bool
  | MRspResult JobResult
  | MRspError String
  | MRspUnknown
  deriving (Show)


instance RspMsg MasterResponseMessage where
  isError (MRspError _) = True
  isError _ = False
  
  getErrorMsg (MRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (MRspUnknown) = True
  isUnknown _ = False
  
  mkErrorMsg e = MRspError e


instance Binary MasterResponseMessage where
  put (MRspSuccess)          = putWord8 1
  put (MRspIsControlling b)  = putWord8 2 >> put b
  put (MRspResult jr)        = putWord8 3 >> put jr
  put (MRspError e)          = putWord8 4 >> put e
  put (MRspUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (MRspSuccess)
        2 -> get >>= \b -> return (MRspIsControlling b)
        3 -> get >>= \jr -> return (MRspResult jr)
        4 -> get >>= \e -> return (MRspError e)
        _ -> return (MRspUnknown)




-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


-- Messages to and from the Worker
data WorkerRequestMessage
  = WReqStartTask TaskData
  | WReqStopTask TaskId
  | WReqStopAllTasks
  | WReqGetActionNames
  | WReqUnknown
  deriving (Show)


instance Binary WorkerRequestMessage where
  put (WReqStartTask td)   = putWord8 1 >> put td
  put (WReqStopTask tid)   = putWord8 2 >> put tid
  put (WReqStopAllTasks)   = putWord8 3
  put (WReqGetActionNames) = putWord8 4
  put (WReqUnknown) = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \td -> return (WReqStartTask td)
        2 -> get >>= \tid -> return (WReqStopTask tid)
        3 -> return (WReqStopAllTasks)
        4 -> return (WReqGetActionNames)
        _ -> return (WReqUnknown)



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


data WorkerResponseMessage
  = WRspSuccess
  | WRspGetActionNames [ActionName]
  | WRspError String
  | WRspUnknown
  deriving (Show)


instance RspMsg WorkerResponseMessage where
  isError (WRspError _) = True
  isError _ = False
  
  getErrorMsg (WRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (WRspUnknown) = True
  isUnknown _ = False

  mkErrorMsg e = WRspError e


instance Binary WorkerResponseMessage where
  put (WRspSuccess)           = putWord8 1
  put (WRspGetActionNames as) = putWord8 2 >> put as
  put (WRspError e)           = putWord8 3 >> put e
  put (WRspUnknown)           = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (WRspSuccess)
        2 -> get >>= \as -> return (WRspGetActionNames as)
        3 -> get >>= \e  -> return (WRspError e)
        _ -> return (WRspUnknown)