-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Master.MasterState
  Copyright  : Copyright (C) 2010 Sebastian Reese
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Master.MasterState 
(
    State
  , States
  , StateResource
  , addResource
  , delResource
  , addWorker
  , delWorker
)where

import qualified Data.List as List
import qualified Data.Map                                as Map
import           System.IO.Unsafe
import           Control.Concurrent.MVar
import           Holumbus.MapReduce.Types
import           Holumbus.Common.FileHandling
import qualified Holumbus.Distribution.Worker.WorkerPort as WP
import           Holumbus.Network.Communication
import           Holumbus.Network.Port

type States = Map.Map IdType State
type State = ([ActionName], WP.WorkerPort)
type StateResource = FilePath

{-# NOINLINE resources #-}
resources :: MVar [StateResource]
resources = unsafePerformIO $ newMVar ([])

addResource :: FilePath -> IO ()
addResource fp = do
  modifyMVar_ resources (return . (fp:))
  updateStats

delResource :: FilePath -> IO ()
delResource fp = do
  modifyMVar_ resources (return . List.delete fp)
  updateStats                      

{-# NOINLINE states #-}
states :: MVar States
states = unsafePerformIO $ newMVar (Map.empty)

addWorker :: IdType -> State -> IO ()
addWorker i state = do
  modifyMVar_ states (return . Map.insert i state)
  updateStats

delWorker :: IdType -> IO ()
delWorker i = do
  modifyMVar_ states (return . Map.delete i)
  updateStats

updateStats :: IO ()
updateStats = do
  resources' <- readMVar resources
  states'    <- readMVar states
  clearResources resources'
  putState resources' states'
    
putState :: [StateResource] -> States -> IO ()
putState ress = mapM_ ( flip printState ress ) . Map.toList
    
printState :: (IdType,State) -> [StateResource] -> IO ()
printState (i,(actions, wp)) = mapM_ (flip appendToTextFile (mkLine i wp actions))

clearResources :: [StateResource] -> IO ()
clearResources = mapM_ (flip writeToTextFile $"")

mkLine :: IdType -> WP.WorkerPort -> [ActionName] -> String
mkLine i wp actions = (show i)++"\t"++(showActions actions)++"\t"++(showWorkerPort wp)++ "\n"

showActions :: [ActionName] -> String
showActions [] = ""
showActions (x:[]) = x
showActions (x:xs) = x ++ " / " ++ showActions xs

showWorkerPort :: WP.WorkerPort -> String
showWorkerPort (WP.WorkerPort (ClientPort (Port _ m))) = case m of
  Nothing                     -> "no socket"
  (Just (SocketId host port)) -> host ++ ":" ++ show port
