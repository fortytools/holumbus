-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Distribution
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Distribution.Distribution
(
-- * Datatypes
  Distribution
  
-- * Creation and Destruction
, newDistribution
, setDistributionWorker
, closeDistribution

-- * Operations
, getMySiteId
, printDebug
)
where

import Control.Concurrent

import qualified Holumbus.Distribution.Master as M
import qualified Holumbus.Distribution.Worker as W
import qualified Holumbus.Distribution.Worker.WorkerPort as WP
import Holumbus.Network.Site



-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data DistributionData = 
   forall m w. (M.Master m, W.Worker w) =>
   DistributionData SiteId m (Maybe w)

type Distribution = MVar DistributionData



-- ---------------------------------------------------------------------------
-- Creation and Destruction
-- ---------------------------------------------------------------------------


newDistribution :: (M.Master m) => m -> IO (Distribution)
newDistribution m
  = do
    sid <- getSiteId
    newMVar (DistributionData sid m emptyNode)
    where
      emptyNode :: Maybe WP.WorkerPort
      emptyNode = Nothing


setDistributionWorker :: (W.Worker w) => w -> Distribution -> IO (Distribution)
setDistributionWorker w dist
  = do
    modifyMVar dist $
      \(DistributionData s m _) -> return (DistributionData s m (Just w), dist)  


--TODO
closeDistribution :: Distribution -> IO ()
closeDistribution _ = return ()



-- ---------------------------------------------------------------------------
-- private helper-functions
-- ---------------------------------------------------------------------------


-- ---------------------------------------------------------------------------
-- public functions
-- ---------------------------------------------------------------------------


getMySiteId :: Distribution -> IO (SiteId)
getMySiteId dist
  = do 
    withMVar dist $
      \(DistributionData s _ _) -> return s

      
printDebug :: Distribution -> IO ()
printDebug dist
  = do
    withMVar dist $
      \(DistributionData s m w) ->
      do
      putStrLn "--------------------------------------------------------"
      putStrLn "Distribtion - internal data\n"
      putStrLn "--------------------------------------------------------"
      putStrLn "SiteId:"
      putStrLn $ show s
      putStrLn "--------------------------------------------------------"
      putStrLn "Master:"
      M.printDebug m
      putStrLn "--------------------------------------------------------"
      putStrLn "Worker:"
      maybe (putStrLn "NOTHING") (\w' -> W.printDebug w') w
      putStrLn "--------------------------------------------------------"      