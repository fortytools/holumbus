-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.DMapReduce
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Distribution.DMapReduce
(
-- * Datatypes
  DMapReduce
, MapReduce(..)

-- * Configuration
, DMRMasterConf(..)
, defaultMasterConfig
, DMRWorkerConf(..)
, defaultWorkerConfig
, DMRClientConf(..)
, defaultClientConfig
  

-- * Creation and Destruction
, mkMapReduceMaster
, mkMapReduceWorker
, mkMapReduceClient

, getMasterRequestPort
)
where

import           Control.Concurrent

import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.MapReduce
import qualified Holumbus.Distribution.Messages as MSG
import qualified Holumbus.Distribution.Master as M
import qualified Holumbus.Distribution.Master.MasterData as MD
import qualified Holumbus.Distribution.Master.MasterPort as MP
import qualified Holumbus.Distribution.Worker as W
import qualified Holumbus.Distribution.Worker.WorkerData as WD
import qualified Holumbus.Distribution.Worker.WorkerPort as WP
import qualified Holumbus.FileSystem.FileSystem as FS
import           Holumbus.Network.Site
import           Holumbus.Network.Port


localLogger :: String
localLogger = "Holumbus.Distribution.DMapReduce"

-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data DMapReduceData = 
   forall m w. (M.Master m, W.Worker w) =>
   DMapReduceData SiteId MapReduceType m (Maybe w)


data DMapReduce = DMapReduce (MVar DMapReduceData)

instance Show DMapReduce where
  show _ = "DMapReduce"



-- ---------------------------------------------------------------------------
-- Configurations
-- ---------------------------------------------------------------------------


data DMRMasterConf = DMRMasterConf {
    msc_StartControlling :: Bool
  , msc_StreamName       :: StreamName
  } 

defaultMasterConfig :: DMRMasterConf
defaultMasterConfig = DMRMasterConf True "MRMaster"


data DMRWorkerConf = DMRWorkerConf {
    woc_StreamName       :: StreamName
  , woc_SocketId         :: Maybe SocketId
  } 

defaultWorkerConfig :: DMRWorkerConf
defaultWorkerConfig = DMRWorkerConf "MRMaster" Nothing


data DMRClientConf = DMRClientConf {
    clc_StreamName       :: StreamName
  , clc_SocketId         :: Maybe SocketId
  } 

defaultClientConfig :: DMRClientConf
defaultClientConfig = DMRClientConf "MRMaster" Nothing


-- ---------------------------------------------------------------------------
-- Creation and Destruction
-- ---------------------------------------------------------------------------

mkMapReduceMaster 
  :: FS.FileSystem -> MapActionMap -> ReduceActionMap -> DMRMasterConf
  -> IO DMapReduce
mkMapReduceMaster fs maps reduces conf
  = do
    let start = msc_StartControlling conf
        sn    = msc_StreamName conf
    sid <- getSiteId
    infoM localLogger $ "initialising master on site " ++ show sid  
    infoM localLogger "creating master"
    md <- MD.newMaster fs maps reduces start sn
    newDMapReduce MRTMaster md (Nothing::Maybe WP.WorkerPort)


mkMapReduceWorker
 :: FS.FileSystem -> MapActionMap -> ReduceActionMap -> DMRWorkerConf
 -> IO DMapReduce
mkMapReduceWorker fs maps reduces conf
  = do
    sid <- getSiteId
    infoM localLogger $ "initialising worker on site " ++ show sid  
    infoM localLogger "creating worker"
    p <- newPort (woc_StreamName conf) (woc_SocketId conf)
    let mp = (MP.newMasterPort p)
    wd <- WD.newWorker fs maps reduces mp
    newDMapReduce MRTWorker mp (Just wd)


mkMapReduceClient 
  :: DMRClientConf
  -> IO DMapReduce
mkMapReduceClient conf
  = do
    sid <- getSiteId
    infoM localLogger $ "initialising map-reduce-client on site " ++ show sid  
    infoM localLogger "creating client"
    p <- newPort (clc_StreamName conf) (clc_SocketId conf) 
    let mp = (MP.newMasterPort p)    
    newDMapReduce MRTClient mp (Nothing::Maybe WP.WorkerPort)
    

newDMapReduce
  :: (M.Master m, W.Worker w)
  => MapReduceType -> m -> Maybe w -> IO DMapReduce
newDMapReduce t m w
  = do
    sid <- getSiteId
    d <- newMVar (DMapReduceData sid t m w)
    return $ DMapReduce d
    

getMasterRequestPort :: DMapReduce -> IO MSG.MasterRequestPort
getMasterRequestPort (DMapReduce mr)
  = withMVar mr $ \(DMapReduceData _ _ m _) -> return $ M.getMasterRequestPort m
    



-- ---------------------------------------------------------------------------
-- public functions
-- ---------------------------------------------------------------------------

instance Debug DMapReduce where

  printDebug (DMapReduce mr)
    = withMVar mr $
        \(DMapReduceData s t m w) ->
        do
        putStrLn "--------------------------------------------------------"
        putStrLn "Distribtion - internal data\n"
        putStrLn "--------------------------------------------------------"
        putStrLn "SiteId:"
        putStrLn $ show s
        putStrLn "Type:"
        putStrLn $ show t
        putStrLn "--------------------------------------------------------"
        putStrLn "Master:"
        printDebug m
        putStrLn "--------------------------------------------------------"
        putStrLn "Worker:"
        maybe (putStrLn "NOTHING") (\w' -> W.printDebug w') w
        putStrLn "--------------------------------------------------------"




instance MapReduce DMapReduce where


  closeMapReduce (DMapReduce mr)
    = withMVar mr $
        \(DMapReduceData _ _ m w) ->
        do
        case w of
          (Just w') -> W.closeWorker w'
          (Nothing) -> return ()
        M.closeMaster m


  getMySiteId (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData s _ _ _) -> return s

  
  getMapReduceType (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData _ t _ _) -> return t

  
  startControlling (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData _ _ m _) -> startControlling m

  
  stopControlling (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData _ _ m _) -> stopControlling m

  
  isControlling (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData _ _ m _) -> isControlling m

  
  doSingleStep (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData _ _ m _) -> doSingleStep m


  doMapReduce ji (DMapReduce mr)
    = withMVar mr $ \(DMapReduceData _ _ m _) -> doMapReduce ji m
